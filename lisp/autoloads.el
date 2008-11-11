(autoload 'js2-mode "js2" nil t)
(autoload 'run-js "inf-js" nil t)
(autoload 'inf-js-keys "inf-js" nil)
(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'run-ruby "inf-ruby" t)
(autoload 'inf-ruby-keys "inf-ruby")
(autoload 'git-status "git" nil t)
(autoload 'clojure-mode "clojure-mode" nil t)
(autoload 'run-clojure "clojure-mode" nil t)

(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'markdown "markdown-mode" nil t)
(autoload 'markdown-preview "markdown-mode" nil t)

(require 'color-theme-autoloads)

(defun color-theme-twilight ()
  (color-theme-initialize)
  (color-theme-twilight))

(defun color-theme-standard ()
  (color-theme-initialize)
  (color-theme-standard))

(require 'semantic-load)
(require 'ecb-autoloads)