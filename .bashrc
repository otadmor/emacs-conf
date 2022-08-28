#  systemctl --user enable emacs
cd () {
    command pushd "$@" > /dev/null
}

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t -a emacs"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs" #  $VISUAL opens in GUI with non-daemon as alternate


# export EDITOR="emacsclient"
# export VISUAL="emacsclient -c"

if [[ "$TERM" == "dumb" ]] ; then
    export COLORTERM=1
    export PYTHONPATH=~/.emacs.d/lisp:$PYTHONPATH
    alias ipython='ipython --colors=LightBG --ext=ipython_emacs_colors'
    if [[ -f ~/.emacs.d/lisp/bash_epc_completion ]]; then
        enable -f ~/.emacs.d/lisp/bash_epc_completion epc_completion
    fi
    # ssh -R 12345:127.0.0.1:$EPC_COMPLETION_SERVER_PORT -o 'SetEnv EPC_COMPLETION_SERVER_PORT=12345' localhost
    # echo "AcceptEnv EPC_COMPLETION_SERVER_PORT" >> /etc/ssh/sshd_config
    # export PYTHONSTARTUP=~/.emacs.d/lisp/py_epc_completion.py
fi

function stfu() {
    $@ >/dev/null 2>&1 &
    disown %%
}

alias babi=nano

alias emacsclient="export DISPLAY=\`[[ \"\$TMUX\" != \"\" ]] && tmux switch-client -r && tmux switch-client -r && tmux show-env DISPLAY 2> /dev/null | grep -oP \"(?<==)(.*)\"  || echo \$DISPLAY\`; emacsclient --display=\$DISPLAY"
alias ec="export DISPLAY=\`[[ \"\$TMUX\" != \"\" ]] && tmux switch-client -r && tmux switch-client -r && tmux show-env DISPLAY 2> /dev/null | grep -oP \"(?<==)(.*)\"  || echo \$DISPLAY\`; stfu emacsclient -c --display=\$DISPLAY"
alias start="export DISPLAY=\`[[ \"\$TMUX\" != \"\" ]] && tmux switch-client -r && tmux switch-client -r && tmux show-env DISPLAY 2> /dev/null | grep -oP \"(?<==)(.*)\"  || echo \$DISPLAY\`; nautilus"
alias bcompare="export DISPLAY=\`[[ \"\$TMUX\" != \"\" ]] && tmux switch-client -r && tmux switch-client -r && tmux show-env DISPLAY 2> /dev/null | grep -oP \"(?<==)(.*)\"  || echo \$DISPLAY\`; bcompare"
alias ps="ps -ww"
fp () {
    grep -RP "$1"
    # find . | xargs -I{} /bin/sh -c "python -c \"import sys; import os; sys.exit(not (os.path.isfile('{}') and b'$1' in open('{}', 'rb').read()))\" && echo {}"
}

ff () {
    find -path "*$1*"
    # find . | grep $1
}

fs () {
    find . -type f -regex '.*\.so*\|.*\.a' -exec nm --print-file-name --defined-only --dynamic {} \; 2> /dev/null | grep $1
    # find -name '*.so*' -exec nm -a --print-file-name --defined-only --dynamic {} \; 2> /dev/null | grep $1
}

fstr () {
    find -type f | xargs strings --print-file-name | grep -E ".*?\:.*$1"
}

grepsymbol () {
    readelf -s "$1" | grep "$2" | awk -v fn="$1" '{print fn ": " $0}'
}

ffs () {
    find . -type f -regex '.*\.so*\|.*\.a' -exec /bin/bash -c "readelf -s \"{}\" | grep \"$1\" | awk -v fn=\"{}\" '{print fn \": \" \$0}'" "{}" "$1" 2> /dev/null \;
}
# readelf -s
#  \| grep $1 \| awk '{print "{}: " $0}'

addr2sym () {
    gdb -nh \
        -ex "file $1" \
        -ex "python sym = gdb.execute('info symbol $2', to_string=True)" \
        -ex "python parse_sym = lambda ssym: ((ssym[0], int(ssym[2])) if len(ssym) == 3 and ssym[2].isdigit() else (ssym[0], 0))" \
        -ex "python sym_info = '%s+%s' % parse_sym(sym[:sym.find(' in section ')].split()) if not sym.startswith('No symbol matches') else 'no symbol'" \
        -ex "python print(sym_info)" \
        -ex "quit" | tail -n 1
}

PROMPT_COMMAND='history -a'
if [[ "$TERM" == "dumb" ]] ; then
    PS1='\e]7;file://\u@\H'"\$PWD"'\e\\\\'"${PS1}"
    # PS1="\$(echo -ne '\033]7;adb://'`getprop ro.serialno`$PWD'\033\\') $PS1"
    alias adb=~/.local/bin/adb_osc7
fi
