vc-git-dired
============

Emacs dired support for git repositories - based on vc-darcs-dired

Compile this into a .elc file and install it in /usr/share/emacs/site-lisp/vc-git-dired.elc,
then when you retrieve a directory in a cloned git repository you will automatically be in
git-dired mode.  There are several bindings with the prefix t - ta to add, t= to see a diff,
tl to see a log, td to mark a file removed.
