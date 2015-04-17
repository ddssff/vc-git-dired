vc-git-dired
============

Emacs dired support for git repositories - based on vc-darcs-dired

Compile this into a .elc file and install it in /usr/share/emacs/site-lisp/vc-git-dired.elc,
then when you retrieve a directory in a cloned git repository you will automatically be in
git-dired mode.  There are several bindings with the prefix t - ta to add, t= to see a diff,
tl to see a log, td to mark a file removed.

Bindings:

 * `t =`: vc-git-dired-whatsnew - Show current edits.  With argument show staged but uncommitted changes.
 * `t a`: vc-git-dired-add-file
 * `t d`: vc-git-dired-remove-file
 * `t l`: vc-git-dired-print-log - Show log.  With argument show only unpushed log entries.
 * `t r`: vc-git-dired-reset-patch - Incomplete

Commit Messages and SSH
=======================

If you are like me, you often start remote emacs processes using ssh -f -X user@host emacs,
and then you want to do git commits inside that emacs.  Unfortunately, it is difficult to
set the EDITOR environment variable in this situation.  I've just found that you can create
a file ~/.ssh/environment on your local machine containing

    EDITOR=emacsclient

and if you then add this line to /etc/ssh/sshd_config on the remote machine:

    PermitUserEnvironment yes

(and run "service ssh restart" there) you will get the environment setting you need.
