;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 4)
(setq js2-mirror-mode nil)

;; C-Mode
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "k&r")))
