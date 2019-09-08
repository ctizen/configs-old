;;; package --- Main init file
;;; Commentary:
;;; Before usage, check: 
;;; 1) the_silver_searcher aka ag is installed in PATH
;;; 2) npm i -g intelephense
;;; 3) For mac: https://objective-see.com/products/lulu.html
;;;    For linux: firejail to isolate intelephense

;;; Code:


(defun reload-init-file ()
    (interactive)
      (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)    ; Reload .emacs file
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(define-key global-map [remap list-buffers] 'buffer-menu-other-window)
(setq lsp-file-watch-ignored
        '(".idea" "node_modules" ".git" "vendor" "build"))
(setq lsp-file-watch-threshold nil)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
(require 'base-theme)
(require 'base-extensions)
(require 'base-functions)
(require 'base-global-keys)

(require 'lang-php)
(require 'lang-javascript)
(require 'tools-treemacs)

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
	centaur-tabs-height 32
	centaur-tabs-set-icons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-set-bar 'left)
  (centaur-tabs-mode t)
  :bind
  ("M-s-<left>" . centaur-tabs-backward)
  ("M-s-<right>" . centaur-tabs-forward))

;; Set default font
(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

;; Start tree view
(treemacs)

;; Some lsp settings
(defun lsp-set-vars ()
  "Set lsp vars."
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-symbol nil))
(add-hook 'lsp-mode-hook #'lsp-set-vars)

;; Git
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(global-hl-line-mode)

(global-set-key (kbd "C-c f") 'counsel-ag)
