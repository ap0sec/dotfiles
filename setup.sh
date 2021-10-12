#!/bin/zsh

BASEDIR=$(dirname $0)
cd $BASEDIR

#
# general
#

for f in .??*; do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".DS_Store" ] && continue

    ln -snfv ${PWD}/"$f" ~/"$f"
done

#
# zprezto
#

for f in $(ls -1 .zprezto/runcoms); do
    [ "$f" = ".DS_Store" ] && continue
    
    ln -snfv ${PWD}/.zprezto/runcoms/"$f" ~/."$f"
done

#
# emacs
#

if type "emacs" > /dev/null 2>&1; then
    ./emacs_build.sh
else
    cat <<EOF
Emacs not installed.
After installing thad, exec: ${PWD}/emacs_build.sh
EOF
fi
