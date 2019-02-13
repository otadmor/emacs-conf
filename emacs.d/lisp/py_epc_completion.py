import os
import sys
import threading
from epc.server import EPCServer
from collections import namedtuple

import sys
import readline
import rlcompleter
readline.set_completer(rlcompleter.Completer().complete)

class EPCCompletionServer(EPCServer):
    def __init__(self, address='localhost', port=0, *args, **kargs):
        EPCServer.__init__(self, (address, port), *args, **kargs)

        def complete(*cargs, **ckargs):
            return self.complete(*cargs, **ckargs)
        self.register_function(complete)

    def print_port(self, stream=None):
        if stream is None:
            stream=sys.stdout
        stream.write('___EPCCompletionServer_PORT=')
        EPCServer.print_port(self, stream)

class PythonModeCompletionServer(EPCCompletionServer):
    def complete(self, *to_complete):
        text = ''.join(list(to_complete))
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except:
            pass
        return tuple(completions)

def __COMPLETER_all_completions(to_complete):
    try:
        return rpc_complete_server.complete(to_complete)
    except:
        return []

def main():
    rpc_complete_server = PythonModeCompletionServer()
    rpc_complete_server.print_port()  # needed for Emacs client
    rpc_complete_thread = threading.Thread(
        target=rpc_complete_server.serve_forever,
        name='PythonModeCompletionServer')
    rpc_complete_thread.setDaemon(True)
    rpc_complete_thread.start()

if os.environ["TERM"] == "dumb":
    main()
