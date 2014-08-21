EMACS = emacs -batch -q -no-site-file -L .

all:
	$(EMACS) -f batch-byte-compile vc-git-dired.el

install:
	mkdir -p $(PREFIX)/usr/share/emacs/site-lisp/
	cp *.elc $(PREFIX)/usr/share/emacs/site-lisp/

clean:
	rm -f *.elc
