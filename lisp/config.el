(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(partial-completion-mode t)
(setq dired-recursive-deletes t)
(column-number-mode t)
(show-paren-mode)
(setq default-abbrev-mode t)
(setq abbrev-file-name (concat emacs-root "abbrevs.def"))
(fset 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'fill-column 80)

; Use cygwin for find on windows.
(if (eq system-type 'windows-nt)
	  (customize-set-variable 'grep-find-template '"c:\\cygwin\\bin\\find . <X> -type f <F> -exec grep <C> -nH -e <R> {} NUL \";\""))

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

(add-to-list 'vc-handled-backends 'Git)

; Key mapping
(define-key global-map (kbd "C-x C-/") 'comment-region)
(define-key global-map (kbd "<f5>") 'rgrep)
(define-key global-map (kbd "C-x C-b") 'buffer-menu)
(define-key global-map (kbd "<f11>") 'frame-fullscreen-mode)

; Dired-X
(setq dired-omit-files "^#\\|\\.$\\|\\.svn")

; Semantic -- part of config for ecb
(setq semantic-load-turn-everything-on t)

