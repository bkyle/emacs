(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(partial-completion-mode t)
(setq dired-recursive-deletes t)
(column-number-mode t)

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
