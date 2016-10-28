(add-to-list 'special-display-buffer-names '("*Completions*" my-display-completions))

(defun my-display-completions (buf)
  "put the *completions* buffer in the right spot"
  (let ((windows (delete (minibuffer-window) (window-list))))
    (if (eq 1 (length windows))
        (progn 
          (select-window (car windows))
          (split-window-vertically)))
    (let ((target-window (window-at 0 (- (frame-height) 2)))
          (pop-up-windows t))
      (set-window-buffer target-window buf)
      target-window)))

(provide 'minibuffer-fix)
