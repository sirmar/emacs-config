(defun back-window ()
  "Go back a window."
  (interactive)
  (other-window -1))

(defun indent-buffer ()
  "Indents whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun goto-last-edit-point ()
  "Sets the cursor on the last edit point."
  (interactive)
  (let ((undos buffer-undo-list))
    (if (listp undos)
        (while (and undos
                    (let ((pos (or (cdr-safe (car undos)) (car undos))))
                      (not (and (integerp pos) (goto-char (abs pos))))))
          (setq undos (cdr undos))))))

(defun sanitize-whitespace ()
  "Converts all tabs to spaces."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))
    (untabify (point-min) (point-max))))

(defun config-buffers ()
  "Create three columns and a bottom grep buffer."
  (interactive)
  (setq w (selected-window))
  (split-window w 176 t)
  (setq w2 (split-window w 50))
  (split-window w 88 t)
  (generate-new-buffer "*grep*")
  (set-window-buffer w2 "*grep*")
  )

(defun move-line-down ()
  "Move current line down and indents"
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
