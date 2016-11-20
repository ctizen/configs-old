(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(setf (cdr (rassoc 'html-mode auto-mode-alist)) 'web-mode)
(setf (cdr (rassoc 'css-mode auto-mode-alist)) 'web-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))
            (define-key web-mode-map (kbd "C-c C-f") nil)
            (define-key web-mode-map (kbd "C-c C-a") nil)
            (define-key web-mode-map (kbd "C-c a") nil)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            ))
