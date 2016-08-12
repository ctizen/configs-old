(require 'package)
(setq package-archives '(
;;                     ("gnu" . "http://elpa.gnu.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")
                     ))
(package-initialize)


(load-file "/usr/share/emacs/site-lisp/cedet/cedet-devel-load.el")
(semantic-load-enable-code-helpers)

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)
(require 'flycheck)

;;(require 'moe-theme)
;;(moe-dark)
;;(moe-theme-set-color 'purple)
(load-theme 'darktooth t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'js-doc)
 (add-hook 'js2-mode-hook
           #'(lambda ()
               (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
               (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; make eslint to be run from node_modules
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;; Manually set params
(global-auto-complete-mode t)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(electric-indent-mode nil)
(global-git-gutter-mode +1)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-linum-mode 1)
(git-gutter:linum-setup)
(global-unset-key (kbd "C-z"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (darktooth)))
 '(custom-safe-themes
   (quote
    ("e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" default)))
 '(ecb-layout-name "top1")
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/projects/online-mobile/" "~/projects/slot/")))
 '(ecb-windows-height 0.15)
 '(fringe-mode 14 nil (fringe))
 '(git-gutter:update-interval 2)
 '(global-git-gutter-mode t)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-expr-indent-offset 0)
 '(typescript-indent-level 2))

(require 'git)
(require 'git-blame)

(require 'auto-indent-mode)

(add-to-list 'load-path
             "/usr/share/emacs/site-lisp/ecb/")
(require 'ecb)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-default-highlight-face ((t (:background "dark violet"))))
 '(ecb-directory-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-history-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-method-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-source-face ((t (:inherit ecb-default-highlight-face :background "dark violet"))))
 '(ecb-tag-header-face ((t (:background "dark green"))))
 '(flycheck-fringe-error ((t (:inherit error :background "red" :foreground "black" :weight bold :width extra-expanded))))
 '(fringe ((t (:background "grey10" :weight bold :width extra-expanded))))
 '(git-gutter:added ((t (:background "lawn green" :foreground "#008700" :weight bold))))
 '(highlight ((t (:background "#4e4e4e" :foreground "plum"))))
 '(js2-error ((t (:background "red" :foreground "black" :weight bold))))
 '(js2-external-variable ((t (:background "orange" :foreground "black" :weight bold))))
 '(minibuffer-prompt ((t (:background "dark slate blue" :foreground "plum")))))

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

(require 'yafolding)
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(add-hook 'after-init-hook
          (lambda ()
            (flycheck-add-mode 'javascript-eslint 'js2-mode)
            (yafolding-mode)            
            ))
            
