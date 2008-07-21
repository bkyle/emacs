;;; -*-Emacs-Lisp-*-
;;;
;;;  $Id: inf-js.el,v 1.6.2.1 2004/07/27 07:51:28 matz Exp $
;;;  $Author: matz $
;;;  $Date: 2004/07/27 07:51:28 $
;;;
;;; Inferior Javascript Mode - javascript process in a buffer.
;;;                            adapted from inf-ruby.el
;;;
;;; Usage:
;;;
;;; (0) check js-program-name variable that can run your environment.
;;;
;;; (1) modify .emacs to use js2-mode (http://code.google.com/p/js2-mode/)
;;;     for example :
;;;
;;;    (autoload 'js2-mode "js2-mode" nil t)
;;;    (add-to-list 'auto-mode-alist '(("\\.js$" . js2-mode)))
;;;    
;;; (2) set to load inf-js and set inf-js key definition in js2-mode.
;;;
;;;    (autoload 'run-js "inf-js" nil t)
;;;    (autoload 'inf-js-keys "inf-js" nil)
;;;    (add-hook 'js2-mode-hook
;;;          '(lambda ()
;;;             (inf-js-keys)))
;;;

(require 'comint)
(require 'compile)

;;
;; you may change these variables
;;
;(defvar js-program-name "rbc --noreadline"
;  "*Program invoked by the run-js command")
;
;(defvar inferior-js-first-prompt-pattern "^rbc0> *"
;  "first prompt regex pattern of js interpreter.")
;
;(defvar inferior-js-prompt-pattern "^\\(rbc.[>*\"'] *\\)+"
;  "prompt regex pattern of js interpreter.")

;;;; for js
(defvar js-program-name "rhino"
  "*Program invoked by the run-js command")

(defvar inferior-js-first-prompt-pattern "^js> *"
  "first prompt regex pattern of js interpreter.")

(defvar inferior-js-prompt-pattern "^js> *"
  "prompt regex pattern of js interpreter.")

;;
;; mode variables
;;
(defvar inferior-js-mode-hook nil
  "*Hook for customising inferior-js mode.")
(defvar inferior-js-mode-map nil
  "*Mode map for inferior-js-mode")

(defconst inferior-js-error-regexp-alist
       '(("SyntaxError: compile error\n^\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
	 ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))


(cond ((not inferior-js-mode-map)
       (setq inferior-js-mode-map
	     (copy-keymap comint-mode-map))
       (define-key inferior-js-mode-map "\C-c\C-l" 'js-load-file)))

(defun inf-js-keys ()
  "Set local key defs for inf-js in js2-mode"
  (define-key js2-mode-map "\M-\C-x" 'js-send-definition)
  (define-key js2-mode-map "\C-c\C-x" 'js-send-definition)
  (define-key js2-mode-map "\C-c\M-x" 'js-send-definition-and-go)
  (define-key js2-mode-map "\C-c\C-r" 'js-send-region)
  (define-key js2-mode-map "\C-c\M-r" 'js-send-region-and-go)
  (define-key js2-mode-map "\C-c\C-b" 'js-send-buffer)
  (define-key js2-mode-map "\C-c\M-b" 'js-send-buffer-and-go)
  (define-key js2-mode-map "\C-c\C-z" 'switch-to-js)
  (define-key js2-mode-map "\C-c\C-l" 'js-load-file)
  (define-key js2-mode-map "\C-c\C-s" 'run-js))


(defvar js-buffer nil "current js process buffer.")

(defun inferior-js-mode ()
  "Major mode for interacting with an inferior js process.

The following commands are available:
\\{inferior-js-mode-map}

A javascript process can be fired up with M-x run-js.

Customisiation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-js-mode-hook (in that order).

You can send text to the inferior javascript process from other buffers containing
Javascript source.
    switch-to-js switches the current buffer to the javascript process buffer.
    js-send-definition sends the current definition to the javascript process.
    js-send-region sends the current region to the javascript process.
    js-send-buffer sends the current buffer to the javascript process.

    js-send-definition-and-go, js-send-region-and-go, js-send-buffer-and-go
        switch to the js process buffer after sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable js-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for js; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
;  Customise in inferior-js-mode-hook
  (setq comint-prompt-regexp "^[^>\n]*>+ *")
  (setq comint-prompt-regexp inferior-js-prompt-pattern)
; (js-mode-variables)
  (setq major-mode 'inferior-js-mode)
  (setq mode-name "Inferior Javascript")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-js-mode-map)
  (setq comint-input-filter (function js-input-filter))
  (setq comint-get-old-input (function js-get-old-input))
  (compilation-shell-minor-mode t)
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist inferior-js-error-regexp-alist)
  (run-hooks 'inferior-js-mode-hook))


(defvar inferior-js-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun js-input-filter (str)
  "Don't save anything matching inferior-js-filter-regexp"
  (not (string-match inferior-js-filter-regexp str)))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun js-get-old-input ()
  "Snarf the sexp ending at point"
  (save-excursion
    (let ((end (point)))
      (re-search-backward inferior-js-first-prompt-pattern)
      (remove-in-string (buffer-substring (point) end)
			inferior-js-prompt-pattern)
      )))

(defun js-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (js-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (js-args-to-list (substring string pos
						 (length string)))))))))

(defun run-js (cmd)
  "Run an inferior Javascript process, input and output via buffer *javascript*.
If there is a process already running in `*javascript*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `js-program-name').  Runs the hooks `inferior-js-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Javascript: " js-program-name)
			 js-program-name)))
  (if (not (comint-check-proc "*javascript*"))
      (let ((cmdlist (js-args-to-list cmd)))
	(set-buffer (apply 'make-comint "javascript" (car cmdlist)
			   nil (cdr cmdlist)))
	(inferior-js-mode)))
  (setq js-program-name cmd)
  (setq js-buffer "*javascript*")
  (pop-to-buffer "*javascript*"))

(defconst js-eval-separator "\n")

(defun js-send-region (start end)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char start)
      (setq line (+ start (forward-line (- start)) 1))
      (goto-char start)
;      compilation-parse-errors parses from second line.
      (save-excursion
        (let ((m (process-mark (js-proc))))
          (set-buffer (marker-buffer m))
          (goto-char m)
          (insert js-eval-separator "\n")
          (set-marker m (point))))
      (comint-send-region (js-proc) start end)
      (comint-send-string (js-proc) "\n"))))

(defun js-send-definition ()
  "Send the current definition to the inferior Javascript process."
  (interactive)
  (save-excursion
    (js2-mark-defun)
    (js-send-region (point) (mark))))

(defun js-send-buffer ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (mark-whole-buffer)
      (js-send-region (mark) (point)))))

(defun switch-to-js (eob-p)
  "Switch to the javascript process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer js-buffer)
      (pop-to-buffer js-buffer)
      (error "No current process buffer. See variable js-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun js-send-region-and-go (start end)
  "Send the current region to the inferior Javascript process.
Then switch to the process buffer."
  (interactive "r")
  (js-send-region start end)
  (switch-to-js t))

(defun js-send-definition-and-go ()
  "Send the current definition to the inferior Javascript. 
Then switch to the process buffer."
  (interactive)
  (js-send-definition)
  (switch-to-js t))

(defun js-send-buffer-and-go ()
  (interactive)
  (js-send-buffer)
  (switch-to-js t))

(defvar js-source-modes '(js2-mode)
  "*Used to determine if a buffer contains Javascript source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a javascript source file by js-load-file.
Used by these commands to determine defaults.")

(defvar js-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last js-load-file command.
Used for determining the default in the 
next one.")

(defun js-load-file (file-name)
  "Load a Javascript file into the inferior Javascript process."
  (interactive (comint-get-source "Load Javscript file: " js-prev-l/c-dir/file
				  js-source-modes t)) ; T because LOAD 
                                                          ; needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq js-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (js-proc) (concat "load(\"" file-name "\");\n")))

(defun js-proc ()
  "Returns the current javascript process. See variable js-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-js-mode)
				      (current-buffer)
				    js-buffer))))
    (or proc
	(error "No current process. See variable js-buffer"))))

;;; Do the user's customisation...

(defvar inf-js-load-hook nil
  "This hook is run when inf-js is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'inf-js-load-hook)

(provide 'inf-js)

;;; inf-js.el ends here
