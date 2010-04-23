(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(partial-completion-mode t)
(setq dired-recursive-deletes t)
(column-number-mode t)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
;(desktop-save-mode 1)
(if (fboundp 'ido-mode) (ido-mode t))
(setq dired-recursive-deletes "top")
(setq compilation-scroll-output t)
(setq split-width-threshold nil)

(require 'uniquify)
(customize-set-variable 'uniquify-buffer-name-style 'forward)

(customize-set-variable 'fill-column 80)

; All this to add support for .m -> .h mapping
(customize-set-variable 'cc-other-file-alist (quote (("\\.m" (".h")) ("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.c\\'" (".h")) ("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".hpp" ".hh" ".h")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))


; Use cygwin for find on windows.
(if (eq system-type 'windows-nt)
	(progn
	  (customize-set-variable 'grep-find-template '"c:\\cygwin\\bin\\find . <X> -type f <F> -exec grep <C> -nH -e <R> {} \";\"")
	  (customize-set-variable 'ispell-program-name '"aspell")
	  (setq find-program "c:\\cygwin\\bin\\find")))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Start the emacs server
(server-start)

; Only enable the current line highlighting when there is a windowing system.
(when window-system
  (tool-bar-mode -1)
  (require 'highlight-current-line)
  (highlight-current-line-on t)
  (set-face-background 'highlight-current-line-face "light yellow")
  (color-theme-standard))

; Tabbing...
(setq-default default-tab-width 4)
(setq-default tab-stop-list (loop for x from 0 to 120 by 4 collect x))
(setq-default indent-line-function 'tab-to-tab-stop)

(add-to-list 'vc-handled-backends 'Git)

; Key mapping
(define-key global-map (kbd "C-x C-/") 'toggle-comment-region)
(define-key global-map (kbd "<f5>") 'rgrep)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-S-r") 'find-file-in-project)
(define-key global-map (kbd "<f11>") 'frame-fullscreen-mode)
(define-key global-map (kbd "C-2") 'list-tags-for-buffer)

; Under 23.1+ on Mac the both Backspace and Delete are mapped to Backspace.
; Also, the command key is set as super instead of meta by default.
(when (and (eq system-type 'darwin)
		 (eq emacs-major-version 23))
  (blink-cursor-mode t)
  (define-key global-map (kbd "<kp-delete>") 'delete-char)
  (setq ns-command-modifier (quote meta))
  (customize-set-variable 'ns-pop-up-frames nil))


; Dired-X
(setq dired-omit-files "^#\\|\\.$\\|\\.svn")

(if window-system
	(color-theme-standard))
