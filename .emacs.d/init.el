(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(load-file "/usr/share/emacs/site-lisp/cedet/cedet-devel-load.el")
(semantic-load-enable-code-helpers)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-to-list 'load-path
             "/usr/share/emacs/site-lisp/ecb/")
(require 'ecb)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("/home/heilage/projects/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file ".emacs.d/elpa/color-theme-modern-20160411.1846/taylor-theme.el")

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(define-key global-map [select] 'end-of-line)

(show-paren-mode 1)
(transient-mark-mode 1)

