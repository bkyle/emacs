
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
  (setq c-style '((java-mode . "bsd"))))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



(defun my-markdown-mode-hook ()
  (configure-tabbing :width 4 :use-tabs nil))

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)