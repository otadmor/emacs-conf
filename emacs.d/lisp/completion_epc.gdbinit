python
ALLOW_COMPLETION_SERVER = False

import ctypes
import os

p_rl_attempted_completion_function = ctypes.c_void_p.in_dll(ctypes.CDLL(None),
    "rl_attempted_completion_function").value
rl_attempted_completion_function_type = ctypes.CFUNCTYPE(
    ctypes.POINTER(ctypes.c_char_p), # restype
    ctypes.c_char_p, ctypes.c_int, ctypes.c_int # argtypes
)
rl_attempted_completion_function = rl_attempted_completion_function_type(
    p_rl_attempted_completion_function
)

def inc_ptr(p, i=1):
    pt = p.__class__
    void_p = ctypes.cast(p, ctypes.c_voidp).value + i * ctypes.sizeof(pt._type_)
    return ctypes.cast(void_p, pt)
# ctypes._Pointer.__add__ = inc_ptr

rl_line_buffer = ctypes.c_char_p.in_dll(ctypes.CDLL(None), "rl_line_buffer")
rl_point = ctypes.c_int.in_dll(ctypes.CDLL(None), "rl_point")


def rl_attempted_completion(s, start=None, end=None):
    if isinstance(s, tuple):
        s = ''.join(s)
    if not isinstance(s, bytes):
        s = bytes(s, 'ascii')
    if start is None:
        start = 0
    assert 0 <= start <= len(s)
    if end is None:
        end = start + len(s)
    assert start <= end <= len(s)
    if b' ' in s:
        _, word = s.rsplit(b' ', 1)
    else:
        word = s
    old_rl_line_buffer = ctypes.cast(rl_line_buffer, ctypes.c_void_p).value
    old_rl_point = rl_point.value
    try:
        rl_point.value = end
        rl_line_buffer.value = ctypes.addressof(
            ctypes.create_string_buffer(s))
        res = rl_attempted_completion_function(
            ctypes.create_string_buffer(word),
            start, 0)
    finally:
        rl_line_buffer.value = old_rl_line_buffer
        rl_point.value = old_rl_point
    if not res:
        return
    completions = []
    while res.contents:
        completions.append(str(ctypes.string_at(res.contents), 'ascii'))
        # completions.append(ctypes.string_at(res.contents))
        res = inc_ptr(res)
    return completions


if os.environ.get("TERM", "") == 'dumb':
    try:
        from collections import namedtuple
        from epc.server import EPCServer
        from epc.client import EPCClient
        import sys
        import threading
    except ImportError:
        def start_completion_thread(epc_port=None):
            # Do nothing when we cannot import the EPC module.
            pass
    else:
        class GDBEPCCompletion(object):
            def complete(self, *to_complete):
                res = rl_attempted_completion(to_complete)
                res = tuple(res)
                return res

        class EPCCompletionServer(EPCServer):
            def __init__(self, address="localhost", port=0, *args, **kargs):
                EPCServer.__init__(self, (address, port), *args, **kargs)

                def complete(*cargs, **ckargs):
                    return self.complete(*cargs, **ckargs)
                self.register_function(complete)

            def print_port(self, stream=None):
                if stream is None:
                    stream = sys.stdout
                stream.write("___EPCCompletionServer_PORT=")
                EPCServer.print_port(self, stream)

        class EPCCompletionClient(EPCClient):
            def __init__(self, address="localhost", port=None, *args, **kargs):
                if port is not None:
                    args = ((address, port),) + args
                EPCClient.__init__(self, *args, **kargs)

                def complete(*cargs, **ckargs):
                    return self.complete(*cargs, **ckargs)
                self.register_function(complete)

        class GDBEPCCompletionServer(EPCCompletionServer, GDBEPCCompletion):
            def __init__(self, *args, **kargs):
                EPCCompletionServer.__init__(self, *args, **kargs)
                GDBEPCCompletion.__init__(self)

        class GDBEPCCompletionClient(EPCCompletionClient, GDBEPCCompletion):
            def __init__(self, *args, **kargs):
                EPCCompletionClient.__init__(self, *args, **kargs)
                GDBEPCCompletion.__init__(self)

        def start_completion_thread(epc_port=None):
            if epc_port is None:
                epc_port = os.environ.get("EPC_COMPLETION_SERVER_PORT", None)
            if epc_port is not None:
                epc_port = int(epc_port)
                rpc_complete = GDBEPCCompletionClient(port=epc_port)
                rpc_complete_thread = threading.Thread(
                    target=rpc_complete.connect,
                    name="PythonModeEPCCompletion",
                    kwargs={'socket_or_address': ("localhost", epc_port)})
            elif ALLOW_COMPLETION_SERVER:
                rpc_complete = GDBEPCCompletionServer()
                if True: # XXX : check if gdb run in batch (-batch) or quiet (-q, -quiet) modes.
                    rpc_complete.print_port()  # needed for Emacs client
                rpc_complete_thread = threading.Thread(
                    target=rpc_complete.serve_forever,
                    name="PythonModeEPCCompletion")
            rpc_complete_thread.daemon = True
            rpc_complete_thread.start()
            return rpc_complete_thread
else:
    def start_completion_thread(epc_port=None):
        # Do nothing as completion-epc is not needed when not running in Emacs.
        pass

start_completion_thread()

end
