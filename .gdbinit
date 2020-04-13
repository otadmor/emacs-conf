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
    import rpyc
except ImportError:
    def get_source_from_ida(lib=None, off=None, callback=None):
        if callback is None:
            return
        else:
            callback(None)
else:
    def _get_source(lib, off):
        if lib.startswith('/'):
            lib = lib[1:]
        import os
        import subprocess
        import time
        import tempfile
        from epc.client import EPCClient
        temp_dir = tempfile.gettempdir()
        server_ports_filename = os.path.join(temp_dir, 'ida_servers.txt')
        base_dir = os.getcwd()
        lib = os.path.join(base_dir, lib)
        error_filename = "%s_idaerror.txt" % (os.path.join(temp_dir, os.path.basename(lib)), )
        try:
            os.unlink(error_filename)
        except:
            pass
        def get_ida_server_port():
            ida_input_filename = lib.replace('\\', '/') + idb_ext
            ida_servers = [l.split(None, 1) for l in open(server_ports_filename, 'r+t').readlines()]
            ida_servers = [port for port, image in ida_servers if image.replace('\n', '') == ida_input_filename]
            assert len(ida_servers) > 0, "%s not found in ida server database" % (ida_input_filename,)
            assert ida_servers[0] != '-', "%s not found in ida server database" % (ida_input_filename,)
            return int(ida_servers[0])
        def run_ida_with_server(lib):
            if os.path.exists(lib + ".til"):
                raise Exception("idb is already opened or not closed correctly", lib)
            elif os.path.exists(lib + idb_ext):
                ida_input_filename = lib + idb_ext
            elif os.path.exists(lib):
                raise Exception("binary found but without idb", lib, idb_ext)
            else:
                raise Exception("binary not found", lib)
            command = [ida_path, '-S%s' % (ida_tmp_script, ), ida_input_filename]
            print('running command: %s' % (' '.join(command),))
            subprocess.Popen(command)
            print('command returned: %s' % (' '.join(command),))
        try:
            port = get_ida_server_port()
        except (FileNotFoundError, AssertionError):
            run_ida_with_server(lib)
        port = None
        for _ in range(10):
            try:
                port = get_ida_server_port()
                if port is not None:
                   time.sleep(0.5)
                   break
            except (FileNotFoundError, AssertionError):
                time.sleep(0.5)
        else:
            try:
                extra_info = open(error_filename, 'rt').read()
            except FileNotFoundError:
                extra_info = "ida seems to start correctly"
            raise Exception("cannot connect to ida server", extra_info)
        print('connect to port %d' % (port,))
        conn = EPCClient(("localhost", port))
        print('connected, requesting offset %X' % (off,))
        try:
            return conn.call_sync('decompile_with_ea_lines', [off,])
        finally:
            conn.close()
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
            self.ida_server.modules.atexit.register(self.ida_server.modules.os.unlink, ida_tmp_script)
            f = self.ida_server.modules.builtins.open(ida_tmp_script, 'wt')
            try:
                f.write(r"""
import idc
import idaapi
from copy import copy
import threading
def load_plugin_decompiler():
    idc.RunPlugin("hexrays", 0)
    idc.RunPlugin("hexarm", 0)
def tag_addrcode(s):
    return (s[0] == idaapi.COLOR_ON and
            s[1] == chr(idaapi.COLOR_ADDR))
def extract_addresses(func, line):
    line = line.line
    anchor = idaapi.ctree_anchor_t()
    addresses = set()
    while len(line) > 0:
        skipcode_index = idaapi.tag_skipcode(line)
        if skipcode_index == 0:  # No code found
            line = line[1:]  # Skip one character ahead
        else:
            if tag_addrcode(line):
                addr_tag = int(line[2:skipcode_index], 16)
                anchor.value = addr_tag
                if anchor.is_citem_anchor() and not anchor.is_blkcmt_anchor():
                    address = func.treeitems.at(addr_tag).ea
                    if address != idaapi.BADADDR:
                        addresses.add(address)
            line = line[skipcode_index:]  # Skip the colorcodes
    return list(addresses)
def get_lines_ea(func):
    if func is None:
        func = ScreenEA()
    if isinstance(func, (int, long, )):
        func = idaapi.decompile(idaapi.get_func(func))
    return [extract_addresses(func, line) for line in func.get_pseudocode()]
def match_lines_asm(offset):
    func = idaapi.decompile(idaapi.get_func(offset))
    lines = get_lines_ea(func)
    s = '\n'.join([("**  "   if offset in addrs else "    ") + l for l, addrs in zip(str(func).splitlines(), lines)])
    return s
def run_ida_server(errors):
    import os
    import threading
    import signal
    from time import sleep
    import tempfile
    import atexit
    from epc.py3compat import SocketServer
    from epc.handler import EPCHandler
    from epc.server import EPCServer
    from epc.utils import newthread
    server_ports_filename = os.path.join(tempfile.gettempdir(), 'ida_servers.txt')
    def update_ida_server_port(db_path, _image_name, port):
        if port is None:
            port = '-'
        else:
            port = str(port)
        _image_name = _image_name.replace('\\', '/')
        with open(db_path, 'w+t') as f:
             f.seek(0)
             should_write_filename = False
             while True:
                 current_pos = f.tell()
                 l = f.readline()
                 l = l.replace('\n', '')
                 if l == '':
                     should_write_filename = True
                     break
                 try:
                     image_name_in_file = l.split(None, 1)[1]
                 except:
                     errors.append("invalid database name %r" % (l,))
                 else:
                     if image_name_in_file == _image_name:
                         break
             f.seek(current_pos)
             f.write(port.ljust(8))
             if should_write_filename:
                 f.write(_image_name + '\n')
    class ThreadingDaemonEPCHandler(EPCHandler):
        def _handle(self, sexp):
            # running this thread makes sure each request from the same user will be in its own thread
            t = newthread(self, target=EPCHandler._handle, args=(self, sexp))
            t.daemon = True
            t.start()
    class ThreadingDaemonEPCServer(SocketServer.ThreadingMixIn, EPCServer):
        # inheriting from ThreadingMixIn makes sure each client request listener has its own thread
        daemon_threads = True
        def __init__(self, *args, **kwds):
            kwds.update(RequestHandlerClass=ThreadingDaemonEPCHandler)
            EPCServer.__init__(self, *args, **kwds)
    ida_server = ThreadingDaemonEPCServer(('localhost', 0))
    @ida_server.register_function
    def decompile_with_ea_lines(offset):
        batch(1)
        try:
            return match_lines_asm(offset)
        finally:
            batch(0)
    ida_server_port = ida_server.server_address[1]
    ida_server_thread = threading.Thread(target=ida_server.serve_forever)
    ida_server_thread.daemon = True
    image_name =  idc.get_idb_path()
    def closeserver(_image_name):
        try:
            update_ida_server_port(server_ports_filename, _image_name, None)
        finally:
            ida_server.shutdown()
            ida_server_thread.join()
    atexit.register(closeserver, image_name)
    update_ida_server_port(server_ports_filename, image_name, ida_server_port)
    ida_server_thread.start()
if __name__ == "__main__":
    errors = []
    batch(1)
    import tempfile
    temp_base_dir = tempfile.gettempdir()
    try:
        if not idaapi.init_hexrays_plugin():
            load_plugin_decompiler()
        idc.auto_wait()
        run_ida_server(errors)
    except:
        try:
            import traceback
            with open("%s_idaerror.txt" % (os.path.join(temp_base_dir, idaapi.get_root_filename()), ), 'wt') as f:
                f.write('cwd = %s\n' % (os.getcwd(),))
                f.write('errors:\n' + '\n'.join(errors))
                traceback.print_exc(file=f)
        finally:
            qexit(0) # idc.Exit(1)
    else:
        try:
            os.unlink("%s_idaerror.txt" % (os.path.join(temp_base_dir, idaapi.get_root_filename()), ))
        except:
            pass
    finally:
        batch(0)
""")
            finally:
                f.close()
            self._get_source = rpyc.utils.classic.teleport_function(self.ida_server, _get_source, globals={
                'ida_tmp_script' : ida_tmp_script,
                'idb_ext' : idb_ext,
                'ida_path' : ida_path,
            })
            self.serve_thread = rpyc.utils.helpers.BgServingThread(self.ida_server)
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