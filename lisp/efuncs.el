
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
