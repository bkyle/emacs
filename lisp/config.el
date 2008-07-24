(setq make-backup-files nil)
(setq vc-make-backup-files nil)
(tool-bar-mode -1)
(partial-completion-mode t)
(setq dired-recursive-deletes t)
(column-number-mode t)

(customize-set-variable 'fill-column 80)

; Use cygwin for find on windows.
(if (eq system-type 'windows-nt)
    (customize-set-variable 'grep-find-command '("c:\\cygwin\\bin\\find . -type f -exec grep -nH -e  {} NUL \";\"" . 48)))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Start the emacs server
(server-start)

; Highlight the current line if running in a windowing system.
(if window-system
  (progn
    (require 'highlight-current-line)
    (highlight-current-line-on t)
    (set-face-background 'highlight-current-line-face "light yellow")))

; Tabbing...
(setq-default default-tab-width 4)

;; The following is only needed (I think) if indent-tabs-mode is set to nil
;; (setq-default tab-stop-list 
;; 			  (funcall (lambda (tab-stop max-stop)
;; 						 (let ((tab-stops '(0)))
;; 						   (while (< (car tab-stops) max-stop)
;; 							 (setq tab-stops (cons (+ tab-stop (car tab-stops)) tab-stops)))
;; 						   (reverse tab-stops))) 
;; 					   (default-value 'default-tab-width)
;; 					   120))

(add-to-list 'vc-handled-backends 'Git)
(autoload 'git-status "git" nil t)