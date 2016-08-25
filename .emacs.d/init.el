(require 'package)
(setq package-archives '(
;;                     ("gnu" . "http://elpa.gnu.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")
                     ))
(package-initialize)


;;(load-file "/usr/share/emacs/site-lisp/cedet/cedet-devel-load.el")
;;(semantic-load-enable-code-helpers)

(setq ns-use-srgb-colorspace nil) ;; for nice powerline arrows
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
(require 'powerline)

(require 'flycheck)
(setq ecb-tip-of-the-day nil)
(setq ring-bell-function 'ignore)
(setq tramp-default-method "ssh")
(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))


(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

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

(defvar *my-ecb-layout-name* "top1")
(when (eq system-type 'darwin)
    (setq *my-ecb-layout-name* "left2"))

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
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (darktooth)))
 '(custom-safe-themes
   (quote
    ("e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" default)))
 '(ecb-layout-name *my-ecb-layout-name*)
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("~/projects/")))
 '(ecb-windows-height 0.15)
 '(ecb-windows-width 0.2)
 '(fringe-mode 14 nil (fringe))
 '(git-gutter:update-interval 2)
 '(global-git-gutter-mode t)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(projectile-global-mode t)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "vendor" "bin" "assets")))
 '(projectile-globally-ignored-file-suffixes nil)
 '(projectile-globally-ignored-files (quote ("TAGS")))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-expr-indent-offset 0)
 '(typescript-indent-level 2))

(require 'git)
(require 'git-blame)

(require 'auto-indent-mode)

(setq ecb-tip-of-the-day nil)

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
 '(minibuffer-prompt ((t (:background "dark slate blue" :foreground "plum"))))
 )

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(define-key global-map [select] 'end-of-line)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

(show-paren-mode 1)
(transient-mark-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-default-font "Terminus-13")
(set-face-attribute 'default t :font "Terminus-13" )
(set-frame-font "Terminus-13" nil t)
;; osx specific
(when (eq system-type 'darwin)
  (set-default-font "Roboto Mono Light for Powerline-13")
  (set-face-attribute 'default t :font "Roboto Mono Light for Powerline-13" )
  (set-frame-font "Roboto Mono Light for Powerline-13" nil t)
)

;; Projectile project manager
(projectile-global-mode)

;; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-hook 'php-mode-hook '(lambda ()
                            (turn-on-ctags-auto-update-mode)
                            (auto-complete-mode t)
                            (require 'ac-php)
                            (setq ac-sources  '(ac-source-php ) )
                            (yas-global-mode 1)
                            (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
                            (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back   ) ;go back
                            ))
;;(require 'flycheck-php)
;;(add-hook 'php-mode-hook 'flycheck-php-load)


;; TIDE setup
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (define-key tide-mode-map "\C-ci" 'js-doc-insert-function-doc)
  (define-key tide-mode-map "@" 'js-doc-insert-tag)

  (set-face-attribute 'flycheck-error nil :foreground "black" :background "red")
  (eldoc-mode +1)
  (auto-complete-mode t)
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
            
(defun open-notepad (filename)
  (interactive "sFile name:> ")
  (find-file (concat "/notepad@furiten.ru#2022:~/notes/" filename))
  )

;; Tabbar
(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)
; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :family "Terminus"
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :family "Terminus"
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :family "Terminus"
 :background "gray50"
 :foreground "white"
 :box '(:line-width 5 :color "gray50" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :family "Terminus"
 :background "purple"
 :foreground "white"
 :underline nil
 :box '(:line-width 5 :color "purple" :style nil))
(set-face-attribute
 'tabbar-button nil
 :family "Terminus"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :family "Terminus"
 :background "gray20"
 :height 0.6)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.5))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)
