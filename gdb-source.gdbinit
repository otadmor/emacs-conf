python
DEFAULT_IDA_SOURCE_LIB = None

class BreakOffsetCommand(gdb.Command):
    """Break at image offset
Usage : bo [image] [offset] {if ...}
        bo [image:hexoffset] {if ...}"""
    def __init__ (self):
        super(BreakOffsetCommand, self).__init__("bo", gdb.COMMAND_BREAKPOINTS)
    def complete(self, arguments_string, last):
        is_brk_char = (len(arguments_string) < len(last))
        args = gdb.string_to_argv(arguments_string)
        if last == '':
            args.append("") # Add dummy argument to complete
        argc = len(args)
        if argc == 1:
            images = get_images()
            # if is_brk_char:
            #     return gdb.COMPLETE_NONE
            if args[0] in images:
                return [args[0], args[0] + ":",] # it's complete
            return images # valid option flags
        elif argc == 2:
            return ["0x" ,]
        elif argc == 3:
            return ["if",]
        elif argc >= 4:
            return gdb.COMPLETE_EXPRESSION
        if is_brk_char:
            return gdb.COMPLETE_NONE
        return [] # No more valid options
    def invoke (self, arg, from_tty):
        args = arg.split()
        if len(args) < 1:
            info(self.__doc__)
            return
        if ':' in args[0]:
            image, offset = args[0].split(':')
            offset = "0x" + offset
            args = [image, offset,] + args[1:]
        if len(args) < 2:
            info(self.__doc__)
            return
        image = args[0]
        offset = gdb.parse_and_eval(args[1])
        conds = ' '.join(args[2:])
        bo(image, offset, conds)
BreakOffsetCommand()

try:
    import epc
    from epc.client import EPCClient
    from functools import partial
    from collections import defaultdict
    from operator import itemgetter
except ImportError:
    def get_source_from_ida(lib=None, off=None, callback=None):
        if callback is None:
            return
        else:
            callback(None)
else:
    def parse_source_results(off, lines, breakpoints):
        lines_addresses = [
            set([addr for _, addr in line[1]])
            for line in lines
        ]
        return [
            (off in line_addresses, line[0], sorted([
                (off == addr, disasm, addr, list(breakpoints[addr]))
                for disasm, addr in line[1]
            ], key=itemgetter(2)))
            for line_addresses, line in zip(lines_addresses, lines)
        ]
    def try_decode_location(location):
        try:
            _, locations = gdb.decode_line(location)
            if locations is None:
                return []
            return locations
        except:
            import traceback; traceback.print_exc()
            return []
    def get_all_breakpoints():
        try:
            return [
                (lo(decoded_address.pc), bp.number)
                for bp in gdb.breakpoints()
                for decoded_address in try_decode_location(bp.location)
            ]
        except:
            import traceback; traceback.print_exc()
            return []
    def get_breakpoints_for_lib(lib):
        break_at_addr = defaultdict(set)
        for (l, addr), no in get_all_breakpoints():
            if l == lib:
                break_at_addr[addr].add(no)
        return break_at_addr
    class IdaRemoteHandler(object):
        def __init__(self):
            self.ida_server = None
            self.in_request = False
            self.requested_params = None
            self.cached_results = {}
        def connect_ida_server(self, ip=None, port=19999, callback=None):
            if ip is None:
                ip = __config__['ida-interact.host'][0]
            if ip is None or ip == "":
                if callback is not None:
                    _, reject = callback
                    return reject("no server")
                else:
                    return
            # run : python -m epc.server --address 0.0.0.0 --port 19999 builtins
            try:
                self.ida_server = EPCClient((ip, port))
            except connectionRefusedError as e:
                if callback is not None:
                    _, reject = callback
                    return reject("cannot connect ida server @ %s:%d" % (ip, port,))
                else:
                    return
            args = ['exec', [open(os.path.expanduser("~/.emacs.d/lisp/gdb_epc.py"), "rt").read().replace(
                '_IDA_INTERNAL_SCRIPT', '"+' + repr(open(os.path.expanduser("~/.emacs.d/lisp/ida_script.py"), "rt").read()) + '+"'
            )]]
            if callback is not None:
                accept, reject = callback
                self.ida_server.call(*args, callback=accept, errback=reject)
            else:
                return self.ida_server.call_sync(*args)
        def cache_results(self, lib, lines):
            addresses = set.union(*(
                set([addr for _, addr in line[1]])
                for line in lines
            ))
            self.cached_results.update({
                (lib, addr) : lines
                for addr in addresses
            })
        def search_in_cache(self, lib, off):
            return self.cached_results.get((lib, off), None)
        def check_ida_server(self):
            return self.ida_server is not None
        def get_source(self, lib, off, callback=None):
            cached = self.search_in_cache(lib, off)
            if cached is not None:
                lib_breakpoints = get_breakpoints_for_lib(lib)
                cached_parsed = parse_source_results(off, cached, lib_breakpoints)
                if callback is not None:
                    accept, _ = callback
                    accept(cached_parsed)
                    return
                else:
                    return cached_parsed
            self.requested_params = (lib, off)
            self.in_request, old_in_request = True, self.in_request
            if old_in_request:
                assert callback is not None, "async request is running in the background, please wait for it to finish and try again"
                accept, _ = callback
                return accept(None)
            if callback is None:
                if not self.check_ida_server():
                    self.connect_ida_server()
                try:
                    lines = self.ida_server.call_sync("get_source", [lib, off])
                    self.cache_results(lib, lines)
                    lib_breakpoints = get_breakpoints_for_lib(lib)
                    return parse_source_results(off, lines, lib_breakpoints)
                finally:
                    self.in_request = False
            else:
                accept, reject = callback
                def queue_job(lib, off):
                    def callback_check_params(is_accept, res):
                        if (lib, off) != self.requested_params:
                            queue_job(*self.requested_params)
                            return
                        self.in_request = False
                        if is_accept:
                            self.cache_results(lib, res)
                            lib_breakpoints = get_breakpoints_for_lib(lib)
                            accept(parse_source_results(off, res, lib_breakpoints))
                        else:
                            reject(res)
                    self.ida_server.call("get_source", [lib, off],
                        callback=partial(callback_check_params, True),
                        errback=partial(callback_check_params, False)
                    )
                if not self.check_ida_server():
                    self.connect_ida_server(callback=(lambda res:queue_job(*self.requested_params), reject))
                else:
                    queue_job(*self.requested_params)
    _ida_remote_handler = IdaRemoteHandler()
    def get_source_from_ida(lib=None, off=None, callback=None):
        global DEFAULT_IDA_SOURCE_LIB
        ip = __config__["ida-interact.host"][0]
        if ip is None or ip == "":
            if callback is not None:
                _, reject = callback
                return reject("no server")
            else:
                return
        if off is None: # assume lib is a virtual address
            lib, off = lo(lib)
        elif lib in ("", None):
            lib, off = DEFAULT_IDA_SOURCE_LIB, off
        return _ida_remote_handler.get_source(lib, off, callback)


class IdaSourceCommand(gdb.Command):
    """Get source from ida
Usage : is [image] [offset]
        is [address]"""
    def __init__ (self):
        super(IdaSourceCommand, self).__init__("is", gdb.COMMAND_USER)
    def complete(self, arguments_string, last):
        is_brk_char = (len(arguments_string) < len(last))
        args = gdb.string_to_argv(arguments_string)
        if last == '':
            args.append("") # Add dummy argument to complete
        argc = len(args)
        if argc == 1:
            images = get_images()
            # if is_brk_char:
            #     return gdb.COMPLETE_NONE
            if args[0] in images:
                return args[0] # it's complete
            images.append("0x")
            return images # valid option flags
        elif argc == 2:
            if '0x' in argument_string:
               return gdb.COMPLETE_NONE
            return ["0x" ,]
        if is_brk_char:
            return gdb.COMPLETE_NONE
        return [] # No more valid options
    def invoke (self, arg, from_tty):
        args = gdb.string_to_argv(arg)
        if len(args) == 2:
            args[1] = gdb.parse_and_eval(args[1])
        elif len(args) not in (2,1,0,):
            info(self.__doc__)
            return
        try:
            source = get_source_from_ida(*args)
            source = '\n'.join([("**  " if r else "    ") + l for r, l, _ in source])
            gef_print(source)
        except Exception as e:
            err(e.args[0])
IdaSourceCommand()

class LibraryOffsetCommand(gdb.Command):
    """Get source from ida
Usage : lo [address]"""
    def __init__ (self):
        super(LibraryOffsetCommand, self).__init__("lo", gdb.COMMAND_USER)
    def invoke (self, arg, from_tty):
        args = gdb.string_to_argv(arg)
        if len(args) == 0:
            args.append(hex(current_arch.pc))
        elif len(args) != 1:
            info(self.__doc__)
            return
        lib, off = lo(gdb.parse_and_eval(args[0]))
        gef_print("%s 0x%x" % (lib, off,))
LibraryOffsetCommand()

end