set max-completions unlimited

python
try:
    gdb.execute("guile (use-modules (gdb))", to_string=True)
    HAS_GUILE = True
except gdb.error:
    HAS_GUILE = False
def rl_attempted_completion(s, start=None, end=None):
    s = ''.join(s)
    s = s.splitlines()[-1] if s.strip() != '' else ''
    if HAS_GUILE:
        x = gdb.execute('guile (execute "complete %s")' % (s.replace('"', '\\"'),), to_string=True)
    else:
        x = gdb.execute('complete ' + s, to_string=True)
    return tuple(x.splitlines())
DISABLED_SOURCE = False

def lo(addr):
    vm = list()
    try:
        fn = next(x.path for x in get_process_maps() if x.page_start <= addr < x.page_end)
    except StopIteration:
        return
    return fn, min([x.page_start for x in get_process_maps() if x.path == fn])

import os
if os.environ.get("TERM", "") == 'dumb':
    try:
        from epc.server import EPCServer
        from epc.client import EPCClient
        from epc.handler import EPCHandler, EPCError
        from epc.core import EPCDispatcher
        import sys
        import threading
        import functools
    except ImportError:
        def start_completion_thread(epc_port=None):
            # Do nothing when we cannot import the EPC module.
            pass
    else:
        def EPCHandler__default_handle_error(this, err):
            if this.handle_error(err):
                return
            if this.server.debugger or this.server.log_traceback:
                exc_info = sys.exc_info()
                this.logger.error('Unexpected error', exc_info=exc_info)
            if this.server.debugger:
                this.server.debugger.post_mortem(exc_info[2])
            name = 'epc-error' if uid is undefined else 'return-error'
            this._send(name, uid, repr(err))
        EPCHandler.default_handle_error = EPCHandler__default_handle_error

        def EPCHandler__handle_call(this, uid, meth, args):
            # See: `epc:handler-called-method`
            name = meth.value()
            try:
                func = this.server.get_method(name)
            except AttributeError:
                return ['epc-error', uid, "EPC-ERROR: No such method : {0}".format(name)]
            if hasattr(func, 'ispromise') and func.ispromise:
                def _accept(res):
                    this._send('return', uid, res)
                try:
                    func(_accept, this.default_handle_error, *args)
                except Exception as err:
                    this.default_handle_error(err)
                return None
            return ['return', uid, func(*args)]
        EPCHandler._handle_call = EPCHandler__handle_call


        def EPCDispatcher__register_promise(this, function, name=None):
            @functools.wraps(function)
            def __wrapper(accept, reject, *args, **kargs):
                return function(accept, reject, *args, **kargs)
            __wrapper.ispromise = True
            return this.register_function(__wrapper, name)
        EPCDispatcher.register_promise = EPCDispatcher__register_promise


        def gdb_promise(function):
            @functools.wraps(function)
            def __inner(accept, reject, *args, **kargs):
                def _promise():
                    try:
                        accept(function(*args, **kargs))
                    except Exception as err:
                        reject(err)
                gdb.post_event(_promise)
            return __inner


        class GDBEPCCompletion(object):
            def __init__(self):
                def complete(*cargs, **ckargs):
                    return self.complete(*cargs, **ckargs)
                self.register_promise(gdb_promise(complete))
                def gdbcommand(*cargs, **ckargs):
                    return self.gdb_command(*cargs, **ckargs)
                self.register_promise(gdb_promise(gdbcommand))
                gdb.events.stop.connect(self.stop_handler)
            def stop_handler(self, event):
                symtabline = gdb.selected_frame().find_sal()
                if symtabline.symtab is not None:
                    filename = symtabline.symtab.fullname()
                    lineno = symtabline.line
                else:
                    filename = None
                    lineno = None
                # print ("completion event type: stop")
                def stop_called(reply):
                    global DISABLED_SOURCE
                    if not DISABLED_SOURCE:
                        gdb.execute('gef config context.layout ""')
                        gdb.execute('gef config context.nb_lines_code 10')
                        gdb.execute('gef config context.nb_lines_code_prev 10')
                        DISABLED_SOURCE = True
                def stop_error(error):
                    if error.args[0] != 'EPC-ERROR: No such method : debugger-stop-event':
                        print(error)
                context = gdb.execute("context regs stack trace", to_string=True)
                code = gdb.execute("context code", to_string=True)
                lib_offset = lo(gdb.selected_frame().pc())
                self.call('debugger-stop-event', [filename, lineno, context, code, lib_offset], callback=stop_called, errback=stop_error)
            def complete(self, *to_complete):
                res = rl_attempted_completion(to_complete)
                res = tuple(res)
                return res
            def gdb_command(self, *args):
                return gdb.execute(''.join(args), to_string=True)

        class EPCCompletionServer(EPCServer):
            def __init__(self, address="localhost", port=0, *args, **kargs):
                EPCServer.__init__(self, (address, port), *args, **kargs)
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
