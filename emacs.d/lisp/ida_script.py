import os
import threading
import signal
from time import sleep
import tempfile
import atexit
import idc
import idaapi
from copy import copy
import threading
from collections import defaultdict
def load_plugin_decompiler():
    idc.RunPlugin("hexrays", 0)
    idc.RunPlugin("hexarm", 0)
def tag_addrcode(s):
    return s[0] == idaapi.COLOR_ON and s[1] == chr(idaapi.COLOR_ADDR)
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
    return addresses
def get_lines_ea(func=None):
    if func is None:
        func = ScreenEA()
    if isinstance(func, (int, long, )):
        func = idaapi.decompile(idaapi.get_func(func))
    lines = [extract_addresses(func, line) for line in func.get_pseudocode()]
    ea_to_lines = defaultdict(set)
    for i, eas in enumerate(lines):
        for ea in eas:
            ea_to_lines[ea].add(i)
    chunk_ea = first_func_chunk(func.entry_ea)
    func_body = func.body
    while chunk_ea != idaapi.BADADDR:
        chunk_end_ea = get_fchunk_attr(chunk_ea, FUNCATTR_END)
        ea = chunk_ea
        while ea != idaapi.BADADDR:
            for i in ea_to_lines[func_body.find_closest_addr(ea).ea]:
                lines[i].add(ea)
            ea = next_head(ea, chunk_end_ea)
        chunk_ea = next_func_chunk(chunk_ea, chunk_ea)
    return [list(line) for line in lines]
def match_lines_asm(offset):
    func = idaapi.decompile(idaapi.get_func(offset))
    lines = get_lines_ea(func)
    return [
        (l, [(idc.GetDisasm(addr), int(addr), ) for addr in addrs])
        for l, addrs in zip(str(func).splitlines(), lines)
    ]
def run_ida_server(errors):
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
