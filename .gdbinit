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
    procs = [p for p in procs if proc_name in p[2]]
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
    import rpyc
except ImportError:
    def get_source_from_ida(lib=None, off=None, callback=None):
        if callback is None:
            return
        else:
            callback(None)
else:
    def _get_source(lib, off):
        import os
        import subprocess
        if lib.startswith('/'):
            lib = lib[1:]
        if os.path.exists(lib):
            ida_input_filename = lib
        elif os.path.exists(lib + idb_ext):
            ida_input_filename = lib + idb_ext
        else:
            raise Exception("idb or binary not found", lib)
        command = [ida_path, '-A', '-S%s 0x%X %s' % (ida_tmp, off, ida_tmp_out, ), ida_input_filename]
        print('running command: %s' % (' '.join(command),))
        subprocess.run(command)
        f = open(ida_tmp_out, 'rt')
        try:
            return f.read()
        finally:
            f.close()

    class IdaRemoteHandler(object):
        def __init__(self):
            self.ida_server = None
            self.current_job = None
            self.current_job_res = None
            self.serve_thread = None
            self._get_source = None
            self.in_request = False
            self.requested_params = None
        def connect_ida_server(self, ip=None, port=18888):
            if ip is None:
               ip = __config__['ida-interact.host'][0]
            # use https://github.com/intezer/docker-ida instead
            self.ida_server = rpyc.classic.connect(ip, port)
            is_64bit = is_elf64()
            if self.ida_server.modules.sys.platform == "linux":
                ida_path = "idat64" if is_64bit else "idat"
            else:
                if is_64bit:
                    ida_paths = self.ida_server.modules.glob.glob(r"c:\program files\ida*\ida64.exe")
                else:
                    ida_paths = self.ida_server.modules.glob.glob(r"c:\program files\ida*\ida.exe")
                if len(ida_paths) == 0:
                    raise Exception("Cannot find ida on ida machine")
                ida_path = ida_paths[0]
            idb_ext = ".i64" if is_64bit else ".idb"
            my_addr = self.ida_server._channel.stream.sock.getpeername()
            addr_string = '%s_%d' % (my_addr[0].replace('.', '_').replace(':', '_'), my_addr[1],)
            remote_tmp_dir = self.ida_server.modules.tempfile.gettempdir()
            ida_tmp_script = self.ida_server.modules.os.path.join(remote_tmp_dir, 'idatmpscript_%s.py' % (addr_string,))
            ida_tmp_out = self.ida_server.modules.os.path.join(remote_tmp_dir, 'idatmpout_%s.txt' % (addr_string,))
            self._get_source = rpyc.utils.classic.teleport_function(self.ida_server, _get_source, globals={
                'ida_tmp' : ida_tmp_script,
                'idb_ext' : idb_ext,
                'ida_path' : ida_path,
                'ida_tmp_out' : ida_tmp_out,
            })
            f = self.ida_server.modules.builtins.open(ida_tmp_script, 'wt')
            try:
                f.write("""
import idc
import idaapi
def load_plugin_decompiler():
    idc.RunPlugin("hexrays", 0)
    idc.RunPlugin("hexarm", 0)
if __name__ == "__main__":
    if len(idc.ARGV) == 3:
        offset = int(idc.ARGV[1], 16)
        if not idaapi.init_hexrays_plugin():
            load_plugin_decompiler()
        try:
            cfunc = idaapi.decompile(offset)
            tl = idaapi.treeloc_t()
            tl.ea = offset
            tl.itp = idaapi.ITP_SEMI
            orig_comment = cfunc.get_user_cmt(tl, idaapi.RETRIEVE_ALWAYS)
            comment_marker = "<---------- HERE"
            if orig_comment is None:
                cfunc.set_user_cmt(tl, comment_marker)
            else:
                cfunc.set_user_cmt(tl, comment_marker + orig_comment)
            try:
                s = str(cfunc)
            finally:
                cfunc.set_user_cmt(tl, orig_comment)
            with open(idc.ARGV[2], 'wt') as f:
                f.write(s)
        except:
            import traceback
            with open(idc.ARGV[2], 'wt') as f:
                traceback.print_exc(file=f)
    idc.Exit(0)
""")
            finally:
                f.close()
            self.serve_thread = threading.Thread(target=self.ida_server.serve_all)
            self.serve_thread.start()
        def check_ida_server(self):
            if not self.ida_server:
                return False
            try:
                self.ida_server.ping()
                return True
            except:
                return False
        def get_source(self, lib, off, callback=None):
            self.requested_params = (lib, off)
            if self.in_request:
                assert callback is not None, "async request is running in the background, please wait for it to finish and try again"
                return callback(None)
            self.in_request = True
            if not self.check_ida_server():
                self.connect_ida_server()
            if callback is None:
                try:
                    return self._get_source(lib, off)
                finally:
                    self.in_request = False
            else:
                lib, off = self.requested_params
                def queue_job(lib, off):
                    if self.current_job is not None:
                        self.current_job = None
                        self.current_job_res = None
                    self.current_job = rpyc.async_(self._get_source)
                    self.current_job_res = self.current_job(lib, off)
                    def callback_check_params(res):
                        if (lib, off) != self.requested_params:
                            queue_job(*self.requested_params)
                            return
                        self.in_request = False
                        callback(res)
                    self.current_job_res.add_callback(callback_check_params)
                queue_job(lib, off)
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