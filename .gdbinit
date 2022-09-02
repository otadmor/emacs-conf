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
        sa, fn = next((x.page_start, x.path) for x in get_process_maps() if x.page_start <= addr < x.page_end)
        if fn in ("", None):
            return fn, addr - sa
    except StopIteration:
        return
    return fn, addr - min([x.page_start for x in get_process_maps() if x.path == fn])

end

source ~/gdb-source.gdbinit

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