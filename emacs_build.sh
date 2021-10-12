#!/bin/zsh

if type "emacs" > /dev/null 2>&1; then
    emacs --batch -f batch-byte-compile ~/.emacs.d/init.el
else
    cat <<EOF
Emacs not installed.
After installing thad, exec: ${PWD}/emacs_build.sh
EOF
fi
