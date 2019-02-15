def __COMPLETER_all_completions(to_complete):
    try:
        return rpc_complete_server.complete(to_complete)
    except:
        return []

import os
import sys
import threading
from epc.server import EPCServer
from collections import namedtuple

import sys
import readline
import rlcompleter
readline.set_completer(rlcompleter.Completer().complete)

def get_func_signature(d):
    symbol = d.func_name
    args = d.func_code.co_varnames[:d.func_code.co_argcount]
    defaults = d.func_defaults
    if defaults is not None:
        dl = len(defaults)
        desc = "%s(%s)" % (symbol, ', '.join(args[:-dl] + tuple("%s=%r" % x for x in zip(args[-dl:], defaults))))
    else:
        desc = "%s(%s)" % (symbol, ', '.join(args))
    return desc

def find_obj(symbol):
    ss = symbol.split(".")
    if len(ss) == 0:
        raise AttributeError("No name")
    n = ss[0]
    gg  = readline.get_completer().im_self.namespace
    if n not in gg:
        raise AttributeError(n)
    d = reduce(lambda d, s: getattr(d, s), ss[1:], gg[n])
    return d

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
    def __init__(self, *args, **kargs):
        EPCCompletionServer.__init__(self, *args, **kargs)

        def meta(*cargs, **ckargs):
            return self.meta(*cargs, **ckargs)
        self.register_function(meta)

        def symbol(*cargs, **ckargs):
            return self.symbol(*cargs, **ckargs)
        self.register_function(symbol)

        def doc(*cargs, **ckargs):
            return self.doc(*cargs, **ckargs)
        self.register_function(doc)

    def readline_complete(self, text):
        i = 0
        while True:
            res = readline.get_completer()(text, i)
            if not res: break
            yield res
            i += 1

    def try_or_err(self, f, err):
        try:
            return f()
        except Exception:
            return err

    def complete(self, *to_complete):
        text = ''.join(list(to_complete))
        try:
            return [{
                'word' : res,
                'doc' : self.try_or_err(lambda:self.doc(res), "Error"),
                'description' : self.try_or_err(lambda:self.meta(res), "Error"),
                'symbol' : self.try_or_err(lambda:self.symbol(res), "Error"),
            } for res in self.readline_complete(text)]
        except:
            import traceback; traceback.print_exc()
            return []

    def meta(self, *candidate):
        return ""

    def symbol(self, *candidate):
        symbol = ''.join(list(candidate))
        if symbol.endswith("("):
            symbol = symbol[:-1]
        try:
            d = find_obj(symbol)
        except AttributeError:
            return ""
        if isinstance(d, type(lambda:None)):
            return get_func_signature(d)
        else:
            return ""

    def doc(self, *candidate):
        symbol = ''.join(list(candidate))
        if symbol.endswith("("):
            symbol = symbol[:-1]
        try:
            d = find_obj(symbol)
        except AttributeError:
            return ""
        return d.__doc__

def py_shell_completion_main():
    rpc_complete_server = PythonModeCompletionServer()
    rpc_complete_server.print_port()  # needed for Emacs client
    rpc_complete_thread = threading.Thread(
        target=rpc_complete_server.serve_forever,
        name='PythonModeCompletionServer')
    rpc_complete_thread.setDaemon(True)
    rpc_complete_thread.start()

if os.environ["TERM"] == "dumb":
    py_shell_completion_main()
