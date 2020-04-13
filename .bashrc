#  systemctl --user enable emacs
cd () {
    command pushd "$@" > /dev/null
}

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t -a emacs"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs --display=localhost:current -a emacs" #  $VISUAL opens in GUI with non-daemon as alternate


export EDITOR="emacsclient"
export VISUAL="emacsclient -c"

if [[ "$TERM" == "dumb" ]] ; then
    alias ipython='ipython --simple-prompt'
    # export PYTHONSTARTUP=~/.emacs.d/lisp/py_epc_completion.py
fi

function stfu() {
    $@ >/dev/null 2>&1 &
    disown %%
}

alias ec="stfu emacsclient -c --display=localhost:current"
alias start=nautilus

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
