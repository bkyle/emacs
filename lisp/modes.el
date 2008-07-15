
;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 4)
(setq js2-mirror-mode nil)
(setq js2-mode-squeeze-spaces nil)
(setq js2-cleanup-whitespace nil)

;; ruby-mode
(autoload 'ruby-mode "ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

; inferior mode for ruby
(autoload 'run-ruby "inf-ruby")
(autoload 'inf-ruby-keys "inf-ruby")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))


;; C-Mode
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "bsd")))

;; text-mode
(add-to-list 'auto-mode-alist '("\\.blog$" . text-mode))
