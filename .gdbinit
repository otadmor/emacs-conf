source ~/.emacs.d/lisp/completion_epc.gdbinit

python
import os
if os.environ.get("TERM", "") == 'dumb':
    gdb.execute("set pagination off")

end