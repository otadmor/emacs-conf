source ~/.emacs.d/lisp/completion_epc.gdbinit

python
import os
if os.environ.get("TERM", "") == 'dumb':
    gdb.execute("set pagination off")

end

python
def get_images():
    return [os.path.basename(x.path) for x in get_process_maps() if x.permission & Permission.EXECUTE]
def __io(image):
    vmmap = get_process_maps()
    return min([x.page_start for x in vmmap if os.path.basename(x.path) == image])
def bo(image, offset, conds):
    addr = __io(image) + offset
    cmd = "b * " + str(addr) + ('' if conds == '' else ' ' + conds)
    gdb.execute(cmd)
class BreakOffsetCommand(gdb.Command):
    """Break at image offset
Usage : bo [image] [offset] {if ...}"""
    def __init__ (self):
        super(BreakOffsetCommand, self).__init__("bo", gdb.COMMAND_BREAKPOINTS)
    def complete(self, arguments_string, last):
        is_brk_char = (len(arguments_string) < len(last))
        args = gdb.string_to_argv(arguments_string)
        if last == '':
            args.append("") # Add dummy argument to complete
        argc = len(args)
        if argc == 1:
            images = get_images()
            # if is_brk_char:
            #     return gdb.COMPLETE_NONE
            if args[0] in images:
                return args[0] # it's complete
            return images # valid option flags
        elif argc == 2:
            return ["0x" ,]
        elif argc == 3:
            return ["if",]
        elif argc >= 4:
            return gdb.COMPLETE_EXPRESSION
        if is_brk_char:
            return gdb.COMPLETE_NONE
        return [] # No more valid options
    def invoke (self, arg, from_tty):
        args = arg.split()
        if len(args) < 2:
            info(self.__doc__)
            return
        image = args[0]
        offset = gdb.parse_and_eval(args[1])
        conds = ' '.join(args[2:])
        bo(image, offset, conds)
BreakOffsetCommand()
end
