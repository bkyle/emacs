;; Set these here even though they don't belong.  For some reason they can't be set
;; from within my-js2-mode-hook.  I assume it has something to do with these variables
;; not being buffer-local.
(setq js2-mirror-mode nil)
(setq js2-mode-squeeze-spaces nil)
(setq js2-cleanup-whitespace nil)


(defun my-js2-mode-hook ()
  "My j2-mode hook"
  (setq tab-width 4)
  (setq js2-basic-offset tab-width)

  ; js2-mode's indentation is good but js-mode's is better.
  (require 'js "javascript.el")
  (setq js-indent-level tab-width)
  (setq indent-line-function 'js-indent-line)

  (inf-js-keys))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)



(defun my-ruby-mode-hook ()
  "My ruby-mode hook"
  (inf-ruby-keys))

(add-hook 'ruby-mode 'my-ruby-mode-hook)



(defun my-c-mode-common-hook ()
  "My c-mode-common-hook"
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (local-set-key (kbd "C-c o") 'ff-find-other-file))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-java-mode-hook ()
  "My java-mode-hook"
  (setq tab-width 4)
  (c-set-style "bsd")
  (setq c-basic-offset tab-width))

(add-hook 'java-mode-hook 'my-java-mode-hook)


(defun my-markdown-mode-hook ()
  (configure-tabbing :width 4 :use-tabs nil))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)


(defun my-dired-load-hook ()
  (load "dired-x"))

(add-hook 'dired-load-hook 'my-dired-load-hook)

(defun my-dired-mode-hook ()
  )

(add-hook 'dired-mode-hook 'my-dired-mode-hook)

(defun my-sgml-mode-hook ()
  (define-key sgml-mode-map "/" (lambda ()
								  (interactive)
								  (or
								   (let (point)
									 (save-excursion
									   (backward-char)
									   (when (looking-at "<")
										 (delete-char 1)
										 (sgml-close-tag)
										 (setq point (point))))
									 (when point
									   (goto-char point)
									   t))
								   (insert "/"))))
  (zencoding-mode))


(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)

(defun my-text-mode-hook ()
  (when (equal major-mode 'text-mode)
	(longlines-mode)
	(flyspell-mode)))

(add-hook 'text-mode-hook 'my-text-mode-hook)


(defun my-ido-setup-hook ()
  (setq ido-enable-flex-matching 1))

(add-hook 'ido-setup-hook 'my-ido-setup-hook)


(defun my-mail-mode-hook ()
  (longlines-mode)
  (flyspell-mode))

(add-hook 'mail-mode-hook 'my-mail-mode-hook)

(defun my-mail-send-hook ()
  ; Have to turn off longlones-mode being sending otherwise the text in the clipboard
  ; will contain the artificial line breaks added by longlines-mode
  (longlines-mode nil))

(add-hook 'mail-send-hook 'my-mail-send-hook)