python
def rl_attempted_completion(s, start=None, end=None):
    s = ''.join(s)
    s = s.splitlines()[-1] if s.strip() != '' else ''
    x = gdb.execute('complete ' + s, to_string=True)
    res = tuple(x.splitlines())
    return res

import os
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
            rpc_complete_thread = None
            if epc_port is not None:
                epc_port = int(epc_port)
                rpc_complete = GDBEPCCompletionClient(port=epc_port)
                rpc_complete_thread = threading.Thread(
                    target=rpc_complete.connect,
                    name="PythonModeEPCCompletion",
                    kwargs={'socket_or_address': ("localhost", epc_port)})
            if rpc_complete_thread is not None:
                rpc_complete_thread.daemon = True
                rpc_complete_thread.start()
            return rpc_complete_thread
else:
    def start_completion_thread(epc_port=None):
        # Do nothing as completion-epc is not needed when not running in Emacs.
        pass

start_completion_thread()

end
