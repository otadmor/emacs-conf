python
import os
if os.environ.get("TERM", "") == 'dumb':
    gdb.execute("set pagination off")
end

source ~/.gdbinit-gef.py

source ~/.emacs.d/lisp/completion_epc.gdbinit

python
def get_images():
    return [os.path.basename(x.path) for x in get_process_maps() if x.permission & Permission.EXECUTE]

def io(image):
    vmmap = get_process_maps()
    return min([x.page_start for x in vmmap if os.path.basename(x.path) == image])

def bo(image, offset, conds):
    addr = io(image) + offset
    cmd = "b * " + str(addr) + ('' if conds == '' else ' ' + conds)
    gdb.execute(cmd)

def get_proc_names():
    return [l.split(None, 2) for l in gdb.execute('info os processes', to_string=True).splitlines()]

def attach_remote_proc_by_name(proc_name):
    procs = get_proc_names()
    procs = [p for p in procs if len(p) >= 3 and proc_name in p[2]]
    if len(procs) == 0:
       raise Exception('no process with name ' + proc_name)
    if len(procs) > 1:
       raise Exception('too many processes with name ' + proc_name)
    gdb.execute('attach %d' % (int(procs[0][0]),))


def lo(addr=None):
    f = gdb.selected_frame()
    if f is None:
        return
    if addr is None:
        addr = f.pc()
    vm = list()
    try:
        fn = next(x.path for x in get_process_maps() if x.page_start <= addr < x.page_end)
    except StopIteration:
        return
    return fn, addr - min([x.page_start for x in get_process_maps() if x.path == fn])


class BreakOffsetCommand(gdb.Command):
    """Break at image offset
Usage : bo [image] [offset] {if ...}"""
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
                return args[0] # it's complete
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
except ImportError:
    def get_source_from_ida(lib=None, off=None, callback=None):
        if callback is None:
            return
        else:
            callback(None)
else:
    def parse_source_results(off, lines):
        lines_addresses = [
            set([addr for _, addr in line[1]])
            for line in lines
        ]
        return [
            (off in line_addresses, line[0], [(off == addr, disasm, addr) for disasm, addr in line[1]])
            for line_addresses, line in zip(lines_addresses, lines)
        ]
    class IdaRemoteHandler(object):
        def __init__(self):
            self.ida_server = None
            self.in_request = False
            self.requested_params = None
            self.cached_results = {}
        def connect_ida_server(self, ip=None, port=19999, callback=None):
            if ip is None:
               ip = __config__['ida-interact.host'][0]
            # run : python -m epc.server --address 0.0.0.0 --port 19999 builtins
            self.ida_server = EPCClient((ip, port))
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
                cached_parsed = parse_source_results(off, cached)
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
                    return parse_source_results(off, lines)
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
                            accept(parse_source_results(off, res))
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
        if off is None: # assume lib is a virtual address
            lib, off = lo(lib)
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
            gef_print(get_source_from_ida(*args))
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

# python
# def have_source(pc=None):
#     if pc is None:
#         pc = current_arch.pc
#     try:
#         return gdb.find_pc_line(pc).symtab.is_valid()
#     except:
#         return False
# ContextCommand__context_source = ContextCommand.context_source
# def ContextCommand__context_source_with_ida(self):
#     if not have_source():
#         gef_print(get_source_from_ida())
#         return
#     ContextCommand__context_source()
# ContextCommand.context_source = ContextCommand__context_source_with_ida
# ContextCommand.layout_mapping["source"] = ContextCommand__context_source_with_ida
# end