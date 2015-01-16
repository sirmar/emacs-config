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

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


