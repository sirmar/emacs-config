(defun delete-grep-header ()
  "Hide grep command in grep buffer."
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 4)
      (end-of-line)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;; (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -n 100 -0 -e grep <C> -nH -E <R>")
(add-hook 'next-error-hook 'recenter)

;; Ignore directories and files
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".*")
     (add-to-list 'grep-find-ignored-files ".*")
     ))
