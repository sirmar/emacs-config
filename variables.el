;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq compile-command "")
(setq tags-file-name "~/TAGS")

(eval-after-load "grep"
  '(progn
    (add-to-list 'grep-find-ignored-directories "_dist")
    (add-to-list 'grep-find-ignored-directories "__deploy")
    (add-to-list 'grep-find-ignored-directories "tmp")
    (add-to-list 'grep-find-ignored-directories "widgetServer")
    ))

