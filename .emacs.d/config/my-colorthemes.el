;; Color themes
(defvar *selected-theme-name* 'alect-dark)

(defun load-my-theme (frame)
  (select-frame frame)
  (setq sml/no-confirm-load-theme t)
  (load-theme *selected-theme-name* t))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-my-theme)
  (load-theme *selected-theme-name* t))
