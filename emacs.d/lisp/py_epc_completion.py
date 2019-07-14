rpc_complete = None
def __COMPLETER_all_completions(to_complete):
    try:
        return rpc_complete.complete(to_complete)
    except:
        return []

import os
import sys
import threading
from epc.server import EPCServer
from epc.client import EPCClient
from collections import namedtuple
import time
import glob
import string
import sys
import readline
import rlcompleter
readline.set_completer(rlcompleter.Completer().complete)

def get_builtin_method_signature(d, without_name=False):
    ddoc = d.__doc__
    ddoc = ddoc[:ddoc.find('\n')]
    ddoc = ddoc[ddoc.find("(") + 1 if without_name else 0:ddoc.find(")") + 1]
    return ddoc

def get_func_signature(d, without_name=False):
    symbol = d.func_name + "(" if not without_name else ""
    args = d.func_code.co_varnames[:d.func_code.co_argcount]
    defaults = d.func_defaults
    if defaults is not None:
        dl = len(defaults)
        desc = "%s%s)" % (symbol, ', '.join(args[:-dl] + tuple("%s=%r" % x for x in zip(args[-dl:], defaults))))
    else:
        desc = "%s%s)" % (symbol, ', '.join(args))
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

class EPCCompletionClient(EPCClient):
    def __init__(self, address='localhost', port=None, *args, **kargs):
        if port is not None:
            args = ((address, port), ) + args
        EPCClient.__init__(self, *args, **kargs)

        def complete(*cargs, **ckargs):
            return self.complete(*cargs, **ckargs)
        self.register_function(complete)

functiontype = type(lambda:None)
class _DUMMY_:
    def __init__(self):
        pass
classtype = type(_DUMMY_)
methodtype = type(_DUMMY_.__init__)
del _DUMMY_

if sys.version_info.major == 2:
    SYMBOL_CHARS = "._" + string.letters + string.digits
    FIRST_SYMBOL_CHARS = string.letters + string.digits
else:
    SYMBOL_CHARS = "._" + string.ascii_letters + string.digits
    FIRST_SYMBOL_CHARS = string.ascii_letters + string.digits
class PythonModeEPCCompletion(object):
    def __init__(self):
        pass

    def register_company_functions(self):
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

    def import_complete(self, text):
        # if not text.startswith("import "):
        #     return
        # text = text[len("import "):]
        imports = [imp.strip() for imp in text.split(",")]
        imp = imports[-1]
        # print ("imp = " + imp)
        for p in sys.path:
            # print("search " + os.path.join(p, imp.replace(".", "/")) + "*")
            res = glob.glob(os.path.join(p, imp.replace(".", "/")) + "*")
            # print(res)
            if len(res) > 0:
                for i in res:
                    yield imp + os.path.splitext(i)[0][len(p) + len(os.path.sep) + len(imp):]
        if sys.version_info.major == 2:
            raise StopIteration

    def from_import_complete(self, text):
        if not (text.startswith("from ") and " import " not in text):
            if sys.version_info.major == 2:
                raise StopIteration
            else:
                return
        if sys.version_info.major == 2:
            raise StopIteration
        if False:
            yield None

    def symbol_or_import_complete(self, text):
        for c in self.readline_complete(text):
            yield c
        for c in self.import_complete(text):
            yield c
        for c in self.from_import_complete(text):
            yield c

    def try_or_err(self, f, err):
        try:
            return f()
        except Exception:
            return err

    def complete(self, *to_complete):
        text = ''.join(list(to_complete))
        pre_text = ''
        if len(text) != 0:
            p = 0
            for i, x in enumerate(text[::-1]):
                if x not in SYMBOL_CHARS:
                    p = i
                    break
            # FIRST_SYMBOL_CHARS
            if len(text) == i:
                return []
            pre_text, text = text[:-p], text[-p:]
        pos = len(pre_text)
        try:
            return [{
                'word' : pre_text + res,
                'pos' : pos,
                'doc' : self.try_or_err(lambda:self.doc(res), "Error"),
                'description' : self.try_or_err(lambda:self.symbol(res)[:100], "Error"),
                'symbol' : self.try_or_err(lambda:self.meta(res), "Error"),
            } for res in set(self.symbol_or_import_complete(text))]
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
        if isinstance(d, functiontype):
            return get_func_signature(d, without_name=True)
        elif isinstance(d, methodtype):
            return get_func_signature(d, without_name=True)
        elif isinstance(d, classtype):
            if hasattr(d, "__init__"):
                return "(" + get_func_signature(d.__init__, without_name=True)
        elif isinstance(d, type(eval)):
            return get_builtin_method_signature(d, without_name=True)
        elif isinstance(d, type(eval.__delattr__)):
            return get_builtin_method_signature(d, without_name=True)
        elif isinstance(d, (list,tuple,)):
            return " = %r" % (d[:100],)
        t = " = %r" % (d,)
        return t
            # if len(t) > 15:
            #     t = t[:13] + "..."
            # return t

    def doc(self, *candidate):
        symbol = ''.join(list(candidate))
        if symbol.endswith("("):
            symbol = symbol[:-1]
        try:
            d = find_obj(symbol)
        except AttributeError:
            return ""
        return d.__doc__


class PythonModeEPCCompletionServer(EPCCompletionServer, PythonModeEPCCompletion):
    def __init__(self, *args, **kargs):
        EPCCompletionServer.__init__(self, *args, **kargs)
        PythonModeEPCCompletion.__init__(self)
        self.register_company_functions()

class PythonModeEPCCompletionClient(EPCCompletionClient, PythonModeEPCCompletion):
    def __init__(self, *args, **kargs):
        EPCCompletionClient.__init__(self, *args, **kargs)
        PythonModeEPCCompletion.__init__(self)
        self.register_company_functions()

def py_shell_completion_main():
    global rpc_complete
    epc_port = os.environ.get("EPC_COMPLETION_SERVER_PORT", None)
    if epc_port is not None:
        epc_port = int(epc_port)
        # print("Connecting completion server, port=%d" % (epc_port,))
        from epc.client import EPCClientHandler
        def EPCClientHandler_handle_error(obj, err):
            # if isinstance(err, (BaseRemoteError, EPCClosed)):
            #     return True
            create_completer_thread(s=True)
            return True
        EPCClientHandler.handle_error = EPCClientHandler_handle_error
        EPCClientHandler_finish_orig = EPCClientHandler.finish
        def EPCClientHandler_finish(obj):
            try:
                EPCClientHandler_finish_orig(obh)
            except Exception as e:
                pass
        EPCClientHandler.finish = EPCClientHandler_finish

        def connect_client(s=False):
            if s:
                time.sleep(1)
            rpc_complete = PythonModeEPCCompletionClient(port=epc_port)
            rpc_complete.connect(socket_or_address=('localhost', epc_port))
        from epc.utils import ThreadedIterator
        ThreadedIterator__target = ThreadedIterator._target
        def ThreadedIterator__target_with_error_handle(obj):
            try:
                ThreadedIterator__target(obj)
            except Exception as e:
                obj.stop()
                create_completer_thread(s=True)
        ThreadedIterator._target = ThreadedIterator__target_with_error_handle

        def create_completer_thread(s):
            rpc_complete_thread = threading.Thread(
                target=connect_client,
                name='PythonModeEPCCompletion',
                kwargs={'s' : False})
            rpc_complete_thread.setDaemon(True)
            rpc_complete_thread.start()
        create_completer_thread(s=False)

if os.environ["TERM"] == "dumb":
    py_shell_completion_main()
