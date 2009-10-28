
;;
;; Tags
;;

;; Stack to contain the tags.
(defvar tag-stack '())

;; Pushes a tag onto the stack and writes it into the buffer at the point.
(defun push-tag (tag)
  (interactive "sTag:")
  (if (and (stringp tag) (> (length tag) 0))
      (progn
        (setq tag-stack (cons tag tag-stack))
        (insert "<"  tag ">"))))

;; Removes the top tag from the stack and writes the closing tag to the buffer at the point.
(defun pop-tag ()
  (interactive)
  (let ((tag (car tag-stack)))
    (if (and (stringp tag) (> (length tag) 0))
        (insert "</" tag ">")))
  (setq tag-stack (cdr tag-stack)))

;; Wraps the region in the specified tag.
(defun wrap-region (tag point mark)
  (interactive "sTag:\nr")
  (save-excursion
    (goto-char point)
    (push-tag tag)
    (goto-char (+ mark (length (concat"</" tag ">")) -1))
    (pop-tag)))

;; Wraps the current line in the specified tag.
(defun wrap-line (tag)
  (interactive "sTag:")
  (save-excursion
    (beginning-of-line)
    (push-tag tag)
    (end-of-line)
    (pop-tag)))

;;
;; Utils
;;

;; Returns the size of the buffer.
(defun size-of-buffer ()
  "Prints the size of the buffer in bytes, kb and mb to the message area."
  (interactive)
  (message (humanize-byte-count (buffer-size))))

(defun size-of-region (start end)
  "Prints the size of the region in bytes, kb and mb to the message area."
  (interactive "r")
  (message (humanize-byte-count (- end start))))

(defun size-of-line ()
  "Prints the size of the line in bytes, kb and mb to the message area."
  (interactive)
  (save-excursion
    (let (start end)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (message (humanize-byte-count (- end start))))))

(defun humanize-byte-count (count)
  "Formats the passed count as bytes, kb and mb"
  (format "%db; %dkb; %dmb" count (/ count 1024) (/ count (* 1024 1024))))

(defun* configure-tabbing (&key (width 4) (use-tabs t))
  (setq indent-tabs-mode use-tabs)
  (setq tab-width width)
  (setq tab-stop-list (build-tab-stop-list width)))

(defun build-tab-stop-list (width)
  "Builds a list of tab stops given the width of the tabs and the maximum column to
specify tabbing to."
  (let ((num-tab-stops (/ 120 width))
		(tab-stops nil)
		(counter 1))
	(while (<= counter num-tab-stops)
	  (setq tab-stops (cons (* counter width) tab-stops))
	  (setq counter (+ 1 counter)))
	(nreverse tab-stops)))

(defun occurances-of-word ()
  "Finds all occurances of the word at the point in the buffer."
  (interactive)
  (save-excursion
	(mark-word)
	(occur (buffer-substring (mark) (point)))))

;;
;; HTML Stuff - Pretty much ripped from http://steve.yegge.googlepages.com/saving-time
;;

(defun html-syntax-highlight (start end)
  (interactive "r")
  (save-excursion
    (let ((text (buffer-substring start end)))
      (with-output-to-temp-buffer "*html-syntax*"
        (set-buffer standard-output)
        (insert "<pre>")
        (save-excursion (insert text))
        (save-excursion (html-escape-text))
        (while (not (eobp))
          (let ((next-change
                 (or (next-single-property-change (point) 'face (current-buffer))
                     (point-max))))
            (html-add-font-tags (point) next-change)
            (goto-char next-change)))
        (insert "</pre>")))))

(defun html-add-font-tags (start end)
  (let (face color rgb name r g b)
    (and
     (setq face (get-text-property start 'face))
     (or (if (listp face) (setq face (car face))) t)
     (setq color (face-attribute face :foreground))
     (setq rgb (assoc (downcase color) color-name-rgb-alist))
     (destructuring-bind (name r g b) rgb
       (let ((text (buffer-substring-no-properties start end)))
         (delete-region start end)
         (insert (format "<span style=\"color:#%.2x%.2x%.2x;\">" (/ r 256) (/ g 256) (/ b 256)))
         (insert text)
         (insert "</span>"))))))

(defun html-escape-text ()
  (dolist (escape 
           '( ("&" . "&amp;")
              ("<" . "&lt;")
              (">" . "&gt;")))
    (save-excursion (replace-string (car escape) (cdr escape)))))

;;
;; Blog Stuff
;;

(defun blog-word-count()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((word-char-count 0))
      (while (not (eobp))
        (unless (looking-at "[ \t\r\n]")
          (incf word-char-count))
        (forward-char))
      (message (format "%d characters, %d words" word-char-count (/ word-char-count 5))))))

(defun blog-make-link (start end url)
  (interactive "r\nsURL:")
  (save-excursion
    (let (f(text (buffer-substring start end)))
      (delete-region start end)
      (insert (format "[%s](%s)" text url)))))

(defun blog-preview (&optional link-p)
  "Previews the blog post in the curr
optional parameter link-p is given a non-nil value the stylesheet will be linked to
the document instead of being included inline."
  (interactive "P")
  (catch 'done
	(let (path stylesheet text)
	  (setq path (file-name-directory (buffer-file-name)))
	  (if (not path)
		  (progn
			(message "Buffer must be saved before previewing")
			(throw 'done nil)))
	  (setq stylesheet (concat path "blog.css"))
	  (if (not (file-exists-p path))
		  (progn
			(message (format "stylesheet does not exist in the directory of the current buffer (%s)" path))
			(throw 'done nil)))
	  (setq text (buffer-substring (point-min) (point-max)))
	  (with-output-to-temp-buffer "*blog-preview*"
		(set-buffer standard-output)
		;; Must wrap <style> in <div> since markdown will only leave inline html block elements alone.
		(insert "<div>")
		(cond
		 (link-p
		  (insert (format "<link rel=\"stylesheet\" type=\"text/css\" media=\"screen\" href=\"%s\">\n\n"
						  stylesheet)))
		 (t
		  ;; Must advance the point the number of characters in the stylesheet since 
		  ;; insert-file-contents won't do this for you.
		  (insert "<style>")
		  (goto-char (+ (point) (second (insert-file-contents stylesheet))))
		  (insert "</style>")))
		(insert "</div>\n\n")
		(insert text)
		(markdown-preview))
	  (kill-buffer "*blog-preview*"))))
	 
;;
;; XFDL Stuff
;;

(defun xfdl-decode-buffer ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (kill-line)
    (base64-decode-region (point-min) (point-max))
    (shell-command-on-region (point-min) (point-max) "gzip -d" "*temp*" t)))

;; Utilities

(defun char-codes-to-string (mark point)
  "Converts a list of character codes separated by word separators into a string and displays them in a
temporary buffer.  This code only works with single-byte characters."

  (interactive "r")
  (save-excursion
	(let ((data (buffer-substring mark point)))
	  (with-output-to-temp-buffer "*string*"
		(set-buffer standard-output)
		(insert data)
		(beginning-of-buffer)
		(while (not (eobp))
		  (let (char-code)
			(push-mark)
			(forward-word)
			(setq char-code (buffer-substring (mark) (point)))
			(delete-region (mark) (min
								   (+ (point) 1)
								   (point-max)))
			(pop-mark)
			(insert (char-to-string (string-to-int char-code)))))))))

(defun revert-all-buffers ()
  (interactive)
  (if (yes-or-no-p "Are you sure that you want to revert *all* buffers? ")
	  (save-excursion
		(dolist (buffer (buffer-list))
		  (if (buffer-file-name buffer)
			  (progn
				(message (format "Reverting %s" (buffer-file-name buffer)))
				(set-buffer buffer)
				(revert-buffer nil t))))))
  (message "Done."))

(defun swap-windows ()
  "Swaps the buffers in the current windows."
  (interactive)
  (catch 'done
	(let (windows window-a window-b buffer-a buffer-b)
	  (setq windows (window-list))

	  (unless (eql 2 (length windows))
		(message "Can only swap 2 windows.")
		(throw 'done t))

	  (setq window-a (car windows))
	  (setq window-b (cadr windows))

	  (setq buffer-a (window-buffer window-a))
	  (setq buffer-b (window-buffer window-b))

	  (set-window-buffer window-a buffer-b)
	  (set-window-buffer window-b buffer-a))))

(defun word-count ()
  (interactive)
  (save-excursion
	(let ((count 0))
	  (beginning-of-buffer)
	  (while (not (eobp))
		(forward-word 1)
		(setq count (+ count 1)))
	  (message (format "%d words" count)))))


;; Carbon Emacs Stuff
(defvar frame-fullscreen-mode nil)

(defun frame-fullscreen-mode ()
  "Makes the frame fullscreen"
  (interactive)
  (cond
   ((not frame-fullscreen-mode)
    (setq frame-fullscreen-mode t)
    (set-frame-parameter nil 'fullscreen 'fullboth))
   (t
    (setq frame-fullscreen-mode nil)
    (set-frame-parameter nil 'fullscreen nil))))


;;
;; Project
;;

(defun find-file-in-project ()
  (interactive)
  (let (files filename)
	(visit-tags-table-buffer)
	(setq files (tags-table-files))
	(setq filename (ido-completing-read "Find File: " files nil))
	(find-file (concat (file-name-directory tags-file-name) filename))))


(defun html-pretty-print (mark point)
  (interactive "r")

  (let ((point-delta 0))

	; tags
	(save-excursion
	  (while (re-search-forward "<" (+ point point-delta) t)
		(save-excursion
		  (goto-char (match-beginning 0))
		  (unless (= (point) 1)
			(backward-char)
			(unless (looking-at "\n")
			  (forward-char)
			  (insert "\n")
			  (incf point-delta))))))

	; attributes
	(save-excursion
	  (while (re-search-forward "\\w+=" (+ point point-delta) t)
		(save-excursion
		  (goto-char (match-beginning 0))
		  (insert "\n")
		  (incf point-delta))))

	; style attribute
	(save-excursion
	  (while (re-search-forward "style=\"\\([^\"]*\\)\"" (+ point point-delta) t)
		(save-excursion
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (goto-char (match-beginning 1))
		  (while (re-search-forward ";" nil t)
			(save-excursion
			  (goto-char (+ (match-beginning 0) 1))
			  (unless (looking-at "\"")
				(insert "\n")
				(incf point-delta))))))))

	; indent the whole thing
	(save-excursion
	  (indent-region mark (+ point point-delta)))))

(defun reload-dot-emacs ()
  "Reloads the emacs configuration."

  (interactive)
  (let ((files '("~/.emacs" "~/_emacs")))
	(dolist (file files)
	  (when (file-exists-p file)
		(load-file file)
		(return)))))

(defun decompile-class ()
  "Decompiles a java class file."
  (interactive)
  (let (data filename)
	(setq data (buffer-substring (point-min) (point-max)))
	(delete-region (point-min) (point-max))
	(setq filename (file-name-nondirectory buffer-file-name))
	(with-temp-file filename
	  (insert data)
	  (set-buffer-file-coding-system 'binary))

	(shell-command (concat "javap -c -l -private " (file-name-sans-extension filename)) (current-buffer))
	(delete-file filename)
	(goto-char (point-min))
	(set-buffer-modified-p nil)))
