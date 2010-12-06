(require 'cl)

(labels ((add-path (p)
		   (add-to-list 'load-path (concat emacs-root p))))

  (add-path "lisp")
  (add-path "site-lisp")
  (add-path "site-lisp/org-7.01h/lisp")
  (add-path "site-lisp/scala")
  (add-path "site-lisp/color-theme-6.6.0"))

(load-library "autoloads")
(load-library "efuncs")
(load-library "config")
(load-library "modes")
(load-library "hooks")
