
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
  (interactive)
  (let ((size (buffer-size))
		  (size-in-kilobytes (/ (buffer-size) 1024))
		  (size-in-megabytes (/ (buffer-size) (* 1024 1024))))
	 (message (concat (int-to-string size) "b; " (int-to-string size-in-kilobytes) "kb; " (int-to-string size-in-megabytes) "mb"))))
    

;;
;; Maven
;;

(defun maven-find-pom (path)
  (let ((current-path (file-name-as-directory path))
        (next-path nil)
        (found-p nil))
    (catch 'done
      (while (not found-p)
        (let ((files (directory-files current-path t))
              (next-path nil))
          (dolist (file files)
            (cond
             ((equal "pom.xml" (file-name-nondirectory file))
              (setq found-p t)
              (throw 'done t))
             ((equal ".." (file-name-nondirectory file))
              (setq next-path file))))
          (if (not next-path)
              (throw 'done t))
          (setq current-path next-path)
          (setq next-path nil))))
    (cond
     (found-p
      (expand-file-name (concat (file-name-as-directory current-path) "pom.xml")))
     (t
      (message "Couldn't find pom.xml")))))


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
