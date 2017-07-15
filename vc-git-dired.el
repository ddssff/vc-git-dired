(require 'vc-git)
(require 'dired)
(require 'dired-aux)
(require 'cl)

(defun drop-prefix (prefix string)
  (cond ((> (length prefix) (length string)) string)
	((string= prefix (substring string 0 (length prefix)))
	 (substring string (length prefix)))
	(t string)))

(defun vc-git-dired-whatsnew (arg)
  "Run \"git diff\" to show unstaged changes.  With an argument runs \"git diff --cached HEAD\" to show staged but uncommited changes."
  (interactive "P")
  (if arg
      (vc-git-command "*vc*" 1 (dired-current-directory) "diff" "--cached" "HEAD")
    (vc-git-command "*vc*" 1 (dired-current-directory) "diff"))
  (with-current-buffer "*vc*" (goto-char 0))
  (display-buffer "*vc*"))

(defun vc-git-dired-print-log (arg)
  "Run \"git log\".  With an argument show unpushed entries only."
  (interactive "P")
  (if arg
      (let* ((inventory-alist (git--inventory (dired-current-directory)))
	     (branch-info (cdr (assoc "." inventory-alist)))
	     (re  "^.*\\.\\.\\.\\([^ ]*\\)\\(.*\\)$")
	     (arg (concat (if (string-match re branch-info)
			      (match-string 1 branch-info)
			    "origin/master")
			  "..HEAD")))
	(vc-git-command "*vc*" 0 nil "log" "--stat" arg))
    (vc-git-command "*vc*" 0 nil "log" "--"))
  (with-current-buffer "*vc*"
    (goto-char 0)
    (if (looking-at "$") (insert "All patches have been pushed")))
  (display-buffer "*vc*"))

(defun vc-git-dired-add-file ()
  "Run \"git add\" on current file."
  (interactive)
  (let ((name (dired-get-filename)))
    (vc-git-command "*vc*" 0 name "add" "--")
    (dired-relist-entry name)
    (dired-goto-file name)))

(defun vc-git-dired-remove-file ()
  "Run \"git rm --cached\" on current file, removing it from the index but leaving it in the tree."
  (interactive)
  (let ((name (dired-get-filename)))
    (vc-git-command "*vc*" 0 name "rm" "--cached")
    (dired-relist-entry name)
    (dired-goto-file name)))

(defun vc-git-dired-reset-patch (arg)
  "Selective reset - run emerge on the edited and head versions of the current file."
  (interactive "P")
  (let* ((top (expand-file-name (vc-git-root default-directory)))
	 (file (dired-get-file-for-visit))
	 (patch (if arg
		    (shell-command-to-string (concat "git diff --cached " file))
		  (shell-command-to-string (concat "git diff " file))))
	 (edited (get-buffer-create "*edited*"))
	 (head (get-buffer-create "*head*")))
    (with-current-buffer edited
      (erase-buffer)
      (insert-file-contents file))
    (with-current-buffer head
      (erase-buffer)
      ;(message (concat "git diff " file " | (cd " top " && patch -s -R -p1 -o -)"))
      (insert (shell-command-to-string (concat "git diff " file " | (cd " top " && patch -s -R -p1 -o -)"))))
    ; On exit write the file unconditionally.
    (add-hook 'quit-hooks `(lambda () (emerge-files-exit ,file)))
    ; We want the head on the left, edited on the right, and all the patches set
    ; to show the edited version.  making edited ancestors puts all patches into perfer-B mode
    (emerge-buffers-with-ancestor head edited head nil quit-hooks)
    ))

(defvar git--dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "tl" 'vc-git-dired-print-log)
    (define-key map "ta" 'vc-git-dired-add-file)
    (define-key map "td" 'vc-git-dired-remove-file)
    (define-key map "t=" 'vc-git-dired-whatsnew)
    (define-key map "tr" 'vc-git-dired-reset-patch)
    map))

(defvar git-dired-do-changes nil
  "If this is true, run 'tla changes' and integrate the result
into the dired output.  This may take some time.")

(defvar git-dired-changes-list nil
  "List of changes for the directory.  This is made buffer local
before it is modified.")

(defun git--set-mode-line ()
  (setq mode-name "GIT"))

(defun git--make-tag (file elem changed)
  ; (message (format "git--make-tag, file: %S, elem: %S" file elem))
  (let* ((typ (cdr elem)))
    (cond ((string= file ".git") "control")
	  ((equal typ '"??") "?? precious")
	  ((equal typ '" A") "_A added")
	  ((equal typ '"A ") "A_ Added")
	  ((equal typ '"AA") "AA ADDED")
	  ((equal typ '" D") "_D removed")
	  ((equal typ '"D ") "D_ Removed")
	  ((equal typ '"DD") "DD REMOVED")
	  ((equal typ '" M") "_M modified")
	  ((equal typ '"M ") "M_ Modified")
	  ((equal typ '"MM") "MM MODIFIED")
	  ((equal typ 0) "")
	  ((null typ) typ)
	  (t typ))))

(progn
  (let ((s "100644 977b6efad5dada05e8bbe51171b87fdda9ce5d6b 0	Text/Markdown.hs"))
    (if (string-match "^...... ........................................ [0-3]\t.*$" s)
	(cons (substring s 50) (substring s 48 49))
      "no match")))

(defun parse-ls-files (b)
  (let ((re "^...... ........................................ [0-3]\t.*$"))
    (mapcar (lambda (s)
	      (if (string-match re s)
		  (cons (substring s 50) (string-to-number (substring s 48 49)))
		(cons s "U")))
	    (split-string (buffer-string) "\n" t))))

(defun parse-git-status-porcelain (b)
  "Turn the argument string into an (path . status) alist.  Strip trailing slash off paths."
  (let ((re "^\\(..\\) \\(\\(.*\\) -> \\)?\\(.*\\)$"))
    (mapcar (lambda (s)
	      (if (string-match re s)
		  (let* ((stat (match-string 1 s))
			 (path (match-string 4 s)))
		    (if (string= stat "##")
			(cons "." path)
		      (cons (if (string-match "^\\(.*\\)/$" path)
				(match-string 1 path)
			      path)
			    stat)))
		(message (format "Unexpected output from git status --porcelain: %S" s))))
	    (split-string (buffer-string) "\n" t))))

(defun git-ls-files-command (buffer directory)
  (with-current-buffer buffer
    (erase-buffer)
    (vc-git-command "*vc*" 0 nil "ls-files" "--full-name" "--others" "--directory" "--stage" directory)
    (parse-ls-files (buffer-string))))

(defun git-status-porcelain (buffer directory)
  "Return an alist of files and their git status.  Includes modified and untracked files only, files that are up to date are not included."
  (with-current-buffer (get-buffer-create buffer)
    (erase-buffer)
    (let ((default-directory directory)
	  (s0 (buffer-string)))
      (vc-git-command buffer 0 nil "status" "-b" "--porcelain" ".")
      (parse-git-status-porcelain (buffer-string)))))

(defun parse-git-remote-show-origin ()
  (let ((re "^  Push  URL: \\(.*\\)$")
	(s (car (cdr (cdr (split-string (buffer-string) "\n" t))))))
    (if (string-match re s)
	(match-string 1 s)
      "nil")))

(defun git-show-remote-origin (buffer directory)
  (with-current-buffer buffer
    (setq default-directory directory)
    (erase-buffer)
    (vc-git-command "*vc*" 0 nil "remote" "show" "-n" "origin" directory)
    (parse-git-remote-show-origin)))

(defun insert-remote-origin (directory)
  (save-excursion
    (let* ((buffer (get-buffer-create "*vc*"))
	   (origin (git-show-remote-origin buffer directory)))
      (goto-line 0)
      ;(end-of-line)
      ;(delete-region (point) (line-end-position))
      (insert (concat origin "  -"))
      )))

(defun git--inventory (directory)
  (let* ((buffer (get-buffer-create "*vc*")))
    (git-status-porcelain buffer directory)))

(defun filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun git-merge-inventory (whatsnew manifest)
  "Merge the manifest and whatsnew to get an inventory."
  (append manifest whatsnew)
  ; Remove entries from manifest if they are in whatsnew
  (append whatsnew (filter (lambda (x) (not (assq (car x) whatsnew))) manifest))
  ;(append whatsnew (mapcar 'list (my-filter (lambda (x) (not (member x (mapcar 'car whatsnew)))) manifest)))
  )

(defun git--dired-hook ()
  "This hook is called every time a file or directory is read, with
the buffer narrowed to the affected listings.  The function reformats
the listings to reflect arch version control"
  (cond ((and (vc-git-root (dired-current-directory))
	      (not (string-match "/.git/" (dired-current-directory))))
	 (message "Getting directory GIT info ... ")
	 (git--set-mode-line)
	 (use-local-map git--dired-mode-map)
	 (let* ((directory (dired-current-directory))
		(top (expand-file-name (vc-git-root directory)))
		(subdir (drop-prefix top directory))
		(inventory-alist (git--inventory directory))
	        ; I don't think git-dired-do-changes has ever been set non-nil,
		; which is why it is ok that git--changes never existed.
		(changes (if git-dired-do-changes (git--changes) git-dired-changes-list)))
	   (if git-dired-do-changes
	       (set (make-local-variable 'git-dired-changes-list) changes))
	   (toggle-read-only -1)
	   (goto-char 0)
	   (insert-remote-origin directory)
	   (goto-line 2)
	   (let ((beg (point))
		 (branch (cdr (assoc "." inventory-alist))))
	     (end-of-line)
	     (delete-region beg (point))
	     (insert "  ")
	     (insert branch))
	   (dired-goto-next-file)
	   (while (dired-move-to-filename)
	     (if (not (or (looking-at "\\.$") (looking-at "\\.\\.$")))
		 (git--edit-dired-line top subdir inventory-alist changes))
	     (dired-next-line 1))
	   (toggle-read-only 1)
	   (message "Getting directory GIT info ... done")
	   ))
	))

(defun git--edit-dired-line (top subdir inventory-alist changes)
  ; (message (format "git--edit-dired-line: top=%S subdir=%S inventory-alist=%S" top subdir inventory-alist))
  (let* ((file (dired-get-filename 'no-dir))
	 ;(elem (assoc (concat subdir file) inventory-alist))
	 (elem (assoc (concat subdir file) inventory-alist))
	 (changed (assoc file changes))
	 (mark (git--make-tag (concat subdir file) elem changed)))
    (beginning-of-line)
    (forward-char 12)
    (let* ((beg (point))
	   (end (- (re-search-forward "[ ]+[^ ]+[ ]+[^ ]+[ ]+[^ ]+[ ]") 1))
	   (fmt (format "%%-%ds" (- end beg))))
      (delete-region beg (- (point) 1))
      (insert (format fmt (or mark ""))))))

(define-derived-mode git-dired-mode
  dired-mode "GIT"
  "Major mode derived from dired-mode for managing GIT project
directories.  Several normal bindings are overridden, and other
bindings starting with 't' are added (see below.)

\\{git--dired-mode-map}"
  ;(message "entering git-dired-mode")
  (set-keymap-parent git--dired-mode-map dired-mode-map)
  ;(add-hook 'dired-after-readin-hook 'git--dired-hook nil t)
  (setq git-dired-mode t)
  ;(message "finished git-dired-mode")
  )

(defun git--dired-before-readin-hook ()
  "If this is a git directory prepare to go into GIT mode."
  (cond ((and (vc-git-root default-directory)
	      (not (string= major-mode 'git-dired-mode)))
	 (setq dired-listing-switches "-la")
	 (git-dired-mode))))

(defun git--look-for-buffer (path)
  "Avoid creating new buffers for a directory when one already exits."
  (dired-find-buffer-nocreate (file-name-as-directory path) 'git-dired-mode))

(if (not (memq 'git--look-for-buffer find-directory-functions))
    (setq find-directory-functions
	  (cons 'git--look-for-buffer find-directory-functions)))

(add-hook 'dired-before-readin-hook 'git--dired-before-readin-hook nil nil)

; Putting git--dired-hook on dired-after-readin-hook seems
; to make emacs really slow, so instead rebind dired "g" to
; run it.
(defun run-git-dired ()
  "Revert dired buffer and call git--dired-hook"
  (interactive)
  (revert-buffer)
  (save-excursion
    (git--dired-hook)))

(define-key dired-mode-map "g" 'run-git-dired)

(provide 'vc-git-dired)
