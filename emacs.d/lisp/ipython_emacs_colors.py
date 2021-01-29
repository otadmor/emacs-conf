import os
import sys
from io import StringIO, BytesIO
from functools import partial
from prompt_toolkit.renderer import print_formatted_text
from prompt_toolkit.output.vt100 import Vt100_Output
from prompt_toolkit.styles import DynamicStyle
from prompt_toolkit.formatted_text import PygmentsTokens

def create_ansi_prompt(ipython):
    ipython._style = ipython._make_style_from_name_or_cls(ipython.highlighting_style)
    ipython.style = DynamicStyle(lambda: ipython._style)
    output_buffer = BytesIO()
    output_buffer.fileno = lambda: sys.stdout.fileno()
    output_buffer.encoding = sys.stdout.encoding
    def _get_size():
        x=os.get_terminal_size()
        return x.lines, x.columns
    output = Vt100_Output(stdout=output_buffer, get_size=_get_size,
                          default_color_depth=ipython.color_depth)
    def get_formatted_text(tokens):
        print_formatted_text(output, tokens, ipython.style)
        res = output_buffer.getvalue()
        output_buffer.seek(0)
        output_buffer.truncate(0)
        return res.decode(sys.stdout.encoding)
    def prompt():
        prompt_text = get_formatted_text(
            PygmentsTokens(ipython.prompts.in_prompt_tokens()))
        lines = [input(prompt_text)]
        prompt_continuation = None
        while ipython.check_complete('\n'.join(lines))[0] == 'incomplete':
            if prompt_continuation is None:
                prompt_continuation = get_formatted_text(
                    PygmentsTokens(ipython.prompts.continuation_prompt_tokens(_get_size()[1])))
            lines.append( input(prompt_continuation) )
        return '\n'.join(lines)
    return prompt

__orig_prompt_for_code = None
__cached_ansi_prompt_for_code = None
def load_ipython_extension(ipython):
    global __orig_prompt_for_code
    if __orig_prompt_for_code is None:
        __orig_prompt_for_code = ipython.prompt_for_code
    if ipython.prompt_for_code == __orig_prompt_for_code:
        global __cached_ansi_prompt_for_code
        if __cached_ansi_prompt_for_code is None:
            __cached_ansi_prompt_for_code = create_ansi_prompt(ipython)
        ipython.prompt_for_code = __cached_ansi_prompt_for_code
def unload_ipython_extension(ipython):
    global __orig_prompt_for_code
    if __orig_prompt_for_code is not None:
        ipython.prompt_for_code = __orig_prompt_for_code
        __orig_prompt_for_code = None
# %colors LightBG
