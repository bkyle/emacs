(setq make-backup-files nil)
(tool-bar-mode nil)
(partial-completion-mode t)
(setq dired-recursive-deletes t)

(if window-system
  (progn
    (require 'highlight-current-line)
    (highlight-current-line-on t)
    (set-face-background 'highlight-current-line-face "light yellow")))

; Tabbing...
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))