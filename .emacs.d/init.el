(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(load-file "/usr/share/emacs/site-lisp/cedet/cedet-devel-load.el")
(semantic-load-enable-code-helpers)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'js-doc)
 (add-hook 'js2-mode-hook
           #'(lambda ()
               (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
               (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; Manually set params
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(electric-indent-mode nil)
(global-git-gutter-mode +1)
(global-linum-mode 1)
(git-gutter:linum-setup)
(global-unset-key (kbd "C-z"))

(require 'git)
(require 'git-blame)

(require 'auto-indent-mode)

(add-to-list 'load-path
             "/usr/share/emacs/site-lisp/ecb/")
(require 'ecb)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (taylor)))
 '(custom-safe-themes
   (quote
    ("deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("/home/heilage/projects/")))
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-expr-indent-offset 0)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "wheat"))))
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-default-highlight-face ((t (:background "dark violet"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-history-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-method-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-source-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-tag-header-face ((t (:background "dark green"))))
 '(js2-error ((t (:background "red" :foreground "black")))))

(load-file ".emacs.d/elpa/color-theme-modern-20160411.1846/taylor-theme.el")

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(define-key global-map [select] 'end-of-line)

(show-paren-mode 1)
(transient-mark-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-default-font "Terminus-13")
(set-face-attribute 'default t :font "Terminus-13" )
(set-frame-font "Terminus-13" nil t)

;; TIDE setup
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (set-face-attribute 'flycheck-error nil :foreground "black" :background "red")
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


