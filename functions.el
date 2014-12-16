;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goes back one window.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun back-window () (interactive) (other-window -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indents whole buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets the cursor on the last edit point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun goto-last-edit-point ()
  (interactive)
  (let ((undos buffer-undo-list))
    (if (listp undos)
        (while (and undos
                    (let ((pos (or (cdr-safe (car undos)) (car undos))))
                      (not (and (integerp pos) (goto-char (abs pos))))))
          (setq undos (cdr undos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converts all tabs to spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sanitize-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))
    (untabify (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup windows in emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun config-buffers ()
    "Create three columns."
    (interactive)
    (setq w (selected-window))
    (split-window w 176 t)
    (setq w2 (split-window w 50))
    (split-window w 88 t)
    (generate-new-buffer "*compilation*")
    (set-window-buffer w2 "*compilation*")
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete help function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-to-character ()
  "Delete text to a specific character."
  (interactive)
  (save-excursion
    (let (p1 p2, toCharacter)
      (setq toCharacter (concat "^" (char-to-string (read-char))))
      (setq p1 (point))
      (skip-chars-forward toCharacter) (setq p2 (point))
      (kill-region p1 p2))))

(defun delete-between-characters ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2, toChar, fromChar)
      (setq toChar (char-to-string (read-char)))
      (setq fromChar toChar)
      (if (string= ">" toChar) (setq fromChar "<"))
      (if (string= "<" toChar) (setq fromChar ">"))
      (if (string= ")" toChar) (setq fromChar "("))
      (if (string= "(" toChar) (setq fromChar ")"))
      (if (string= "}" toChar) (setq fromChar "{"))
      (if (string= "{" toChar) (setq fromChar "}"))
      (if (string= "]" toChar) (setq fromChar "["))
      (if (string= "[" toChar) (setq fromChar "]"))
      (skip-chars-backward (concat "^" fromChar)) (setq p1 (point))
      (skip-chars-forward (concat "^" toChar)) (setq p2 (point))
      (kill-region p1 p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move line help function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-line-down ()
  "Move current line down one line and indents"
   (interactive)
   (next-logical-line)
   (transpose-lines 1)
   (previous-logical-line 2)
   (indent-for-tab-command)
   (next-logical-line)
   (indent-for-tab-command))

(defun move-line-up ()
  "Move current line up and indents"
  (interactive)
  (transpose-lines 1)
  (previous-logical-line)
  (indent-for-tab-command)
  (previous-logical-line)
  (indent-for-tab-command))

