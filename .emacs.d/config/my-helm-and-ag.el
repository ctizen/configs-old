;; Helm & ag
;; If async is installed
(add-to-list 'load-path "~/.emacs.d/vendor/helm")
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-async")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-a") 'helm-projectile-ag)
(global-set-key (kbd "C-c a") (lambda () (interactive) (helm-projectile-find-file-dwim)))
(global-set-key (kbd "C-c C-f") 'helm-find-files)

(setq helm-always-two-windows t)
(setq helm-autoresize-mode t)
