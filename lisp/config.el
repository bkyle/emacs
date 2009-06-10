(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(tool-bar-mode -1)
;(menu-bar-mode -1)
(partial-completion-mode t)
(setq dired-recursive-deletes t)
(column-number-mode t)
(show-paren-mode)
(setq default-abbrev-mode t)
(setq abbrev-file-name (concat emacs-root "abbrevs.def"))
(fset 'yes-or-no-p 'y-or-n-p)
(desktop-save-mode 1)
(ido-mode)
(setq dired-recursive-deletes "top")
(setq compilation-scroll-output t)

(require 'uniquify)
(customize-set-variable 'uniquify-buffer-name-style 'forward)

(customize-set-variable 'fill-column 80)

; Use cygwin for find on windows.
(if (eq system-type 'windows-nt)
	(progn
	  (customize-set-variable 'grep-find-template '"c:\\cygwin\\bin\\find . <X> -type f <F> -exec grep <C> -nH -e <R> {} NUL \";\"")
	  (customize-set-variable 'ispell-program-name '"aspell")
	  (setq find-program "c:\\cygwin\\bin\\find")))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Start the emacs server
(server-start)

; Only enable the current line highlighting when there is a windowing system.
(when window-system
  (require 'highlight-current-line)
  (highlight-current-line-on t)
  (set-face-background 'highlight-current-line-face "light yellow"))

; Tabbing...
(setq-default default-tab-width 4)
(setq-default tab-stop-list (loop for x from 0 to 120 by 4 collect x))
(setq-default indent-line-function 'tab-to-tab-stop)

(add-to-list 'vc-handled-backends 'Git)

; Key mapping
(define-key global-map (kbd "C-x C-/") 'comment-region)
(define-key global-map (kbd "<f5>") 'rgrep)
(define-key global-map (kbd "C-x C-b") 'buffer-menu)
(define-key global-map (kbd "C-S-r") 'find-file-in-project)
(define-key global-map (kbd "<f11>") 'frame-fullscreen-mode)


; Dired-X
(setq dired-omit-files "^#\\|\\.$\\|\\.svn")
