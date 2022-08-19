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

if [[ "$TERM" != "dumb" ]] && [[ "$TMUX" != "" ]]; then
    bind -x '"\C-x\e[C":"tmux select-pane -L"'
    bind -x '"\C-x\e[D":"tmux select-pane -R"'
    bind -x '"\C-x\e[A":"tmux select-pane -U"'
    bind -x '"\C-x\e[B":"tmux select-pane -D"'
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

fp () {
    grep -RP "$1"
}

ff () {
    find -path "*$1*"
}

fs () {
    find -name '*.so*' -exec nm -a --print-file-name --defined-only --dynamic {} \; 2> /dev/null | grep $1
}

fstr () {
    find -type f | xargs strings --print-file-name | grep -E ".*?\:.*$1"
}

addr2sym () {
    gdb -nh \
        -ex "file $1" \
        -ex "python sym = gdb.execute('info symbol $2', to_string=True)" \
        -ex "python parse_sym = lambda ssym: ((ssym[0], int(ssym[2])) if len(ssym) == 3 and ssym[2].isdigit() else (ssym[0], 0))" \
        -ex "python sym_info = '%s+%s' % parse_sym(sym[:sym.find(' in section ')].split()) if not sym.startswith('No symbol matches') else 'no symbol'" \
        -ex "python print(sym_info)" \
        -ex "quit" | tail -n 1
}
