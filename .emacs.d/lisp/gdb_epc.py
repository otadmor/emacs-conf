if '_global_servers' not in globals():
    print('loading new global servers dict')
    _global_servers = {}

import sys
import glob
import os
import tempfile

if sys.platform == "linux":
    ida_paths, ida64_paths = ["ida",], ["ida64",]
else:
    ida_paths = glob.glob(r"c:\program files\ida*\ida.exe")
    ida64_paths = glob.glob(r"c:\program files\ida*\ida64.exe")
if len(ida_paths) == 0 or len(ida64_paths) == 0:
    raise Exception("Cannot find ida on ida machine")
temp_dir = tempfile.gettempdir()
server_ports_filename = os.path.join(temp_dir, 'ida_servers.txt')
# atexit.register(os.unlink, server_ports_filename)

def get_source(lib, off):
    import os
    import subprocess
    import atexit
    import time
    import random
    import string
    from epc.client import EPCClient
    global temp_dir
    global _global_servers

    def get_ida_server_port(lib):
        global idb_ext
        global server_ports_filename
        lib = lib.replace('\\', '/')
        ida_input_filename = (lib + ".idb", lib + ".i64")
        ida_servers = [l.split(None, 1) for l in open(server_ports_filename, 'r+t').readlines()]
        ida_servers = [port for port, image in ida_servers if image.replace('\n', '') in ida_input_filename]
        assert len(ida_servers) > 0, "%s not found in ida server database" % (','.join(ida_input_filename),)
        assert ida_servers[0] != '-', "%s not found in ida server database" % (','.join(ida_input_filename),)
        return int(ida_servers[0])
    def run_ida_with_server(lib, client_identifier):
        global ida_paths
        global ida64_paths
        if os.path.exists(lib + ".til"):
            raise Exception("idb is already opened or not closed correctly", lib)
        elif os.path.exists(lib + ".idb"):
            ida_input_filename = lib + ".idb"
            _ida_path = ida_paths[0]
        elif os.path.exists(lib + ".i64"):
            ida_input_filename = lib + ".i64"
            _ida_path = ida64_paths[0]
        elif os.path.exists(lib):
            raise Exception("binary found but without idb", lib)
        else:
            raise Exception("binary not found", lib)
        ida_tmp_script = os.path.join(temp_dir, 'idatmpscript_%s.py' % (client_identifier,))
        command = [_ida_path, '-S%s' % (ida_tmp_script, ), ida_input_filename]
        atexit.register(os.unlink, ida_tmp_script)
        with open(ida_tmp_script, 'wt') as f:
            f.write("_IDA_INTERNAL_SCRIPT")
        print('running command: %s' % (' '.join(command),))
        def run_and_delete(args, filename):
            proc = subprocess.Popen(args)
            proc.wait()
            os.unlink(filename)
        threading.Thread(target=run_and_delete, args=(command, ida_tmp_script)).start()
        print('command returned: %s' % (' '.join(command),))

    def generate_random_string(len):
        return ''.join(random.choice(string.ascii_letters) for i in range(len))

    if lib.startswith('/'):
        lib = lib[1:]
    base_dir = os.getcwd()
    lib = os.path.join(base_dir, lib)
    error_filename = "%s_idaerror.txt" % (os.path.join(temp_dir, os.path.basename(lib)), )
    try:
        os.unlink(error_filename)
    except:
        pass
    try:
        port = get_ida_server_port(lib)
    except (FileNotFoundError, AssertionError):
        client_identifier = generate_random_string(6)
        run_ida_with_server(lib, client_identifier)
    port = None
    for _ in range(10):
        try:
            port = get_ida_server_port(lib)
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
    expected_server_address = ("localhost", port)
    if lib in _global_servers:
        current_connection = _global_servers[lib]
        if current_connection.socket.getpeername() != expected_server_address:
            current_connection.close()
            del _global_servers[lib]
    if lib not in _global_servers:
        print('connect to port %d' % (port,))
        _global_servers[lib] = EPCClient(expected_server_address)
    print('requesting offset %X' % (off,))
    return _global_servers[lib].call_sync('decompile_with_ea_lines', [off,])
import builtins
setattr(builtins, "get_source", get_source)
print('done script')
