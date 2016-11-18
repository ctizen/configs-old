(require 'package)
(setq package-archives '(
;;                     ("gnu" . "http://elpa.gnu.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")
                     ))
(package-initialize)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(run-at-time nil (* 5 60) 'recentf-save-list)

(defvar *my-browser* 'browse-url-chromium)
(when (eq system-type 'darwin)
    (setq *my-browser* 'browse-url-default-macosx-browser))

(require 'popwin)
(popwin-mode 1)

;; Color themes
(defvar *selected-theme-name* 'alect-dark)

(defun load-my-theme (frame)
  (select-frame frame)
  (setq sml/no-confirm-load-theme t)
  (load-theme *selected-theme-name* t))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-my-theme)
  (load-theme *selected-theme-name* t))

;; Fonts
(set-frame-font "Terminus-11")
(add-to-list 'default-frame-alist '(font . "Terminus-11"))
(set-face-attribute 'default t :font "Terminus-11" )
(set-frame-font "Terminus-11" nil t)
;; osx specific
(when (eq system-type 'darwin)
  (set-frame-font "Terminus (TTF) for Powerline-16")
  (set-face-attribute 'default t :font "Terminus (TTF) for Powerline-16" )
  (set-frame-font "Terminus (TTF) for Powerline-16" nil t)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3C3836" "#FB4934" "#84BB26" "#FABD2F" "#83A598" "#D3869B" "#3FD7E5" "#EBDBB2"])
 '(browse-url-browser-function *my-browser*)
 '(custom-safe-themes
   (quote
    ("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "0eea76fe89061a7f6da195f4a976c0b91150de987b942fac2dd10992aea33833" "5a0930a84612f861bb5e98999a50ec6ef7995676c7330aac9b8deda1aaa45f83" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" default)))
 '(ecb-layout-name "left14")
 '(ecb-layout-window-sizes nil)
 '(ecb-minor-mode-text "")
 '(ecb-options-version "2.50")
 '(ecb-source-path (quote (("~/projects" "Workspace"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-height 0.15)
 '(ecb-windows-width 0.2)
 '(fci-rule-color "#d6d6d6")
 '(fringe-mode 14 nil (fringe))
 '(git-gutter:modified-sign "%")
 '(git-gutter:update-interval 2)
 '(global-git-gutter-mode t)
 '(helm-always-two-windows t)
 '(helm-autoresize-mode t)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(line-spacing 0.2)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-headers-fields
   (quote
    ((:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:maildir . 10)
     (:from . 22)
     (:subject))))
 '(mu4e-html2text-command (quote mu4e-shr2text))
 '(mu4e-mu-binary "~/.emacs.d/mu4e/mu-binary-mac")
 '(mu4e-sent-folder "/Sent")
 '(mu4e-sent-messages-behavior (quote delete))
 '(mu4e-trash-folder "/Trash")
 '(mu4e-update-interval 900)
 '(mu4e-view-show-images t)
 '(package-selected-packages
   (quote
    (alect-themes afternoon-theme color-theme-sanityinc-solarized mew popwin web-mode tabbar rich-minority restclient markdown-preview-mode magit-popup js3-mode js2-mode js-doc iedit helm-projectile helm-ag git-gutter git-commit git-blame git flymake-php find-file-in-repository fic-mode exec-path-from-shell ecb darktooth-theme ctags-update color-theme-solarized color-theme-modern calfw auto-indent-mode ag)))
 '(pos-tip-background-color "color-23")
 '(pos-tip-foreground-color "color-230")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "vendor" "bin" "assets")))
 '(projectile-globally-ignored-file-suffixes nil)
 '(projectile-globally-ignored-files (quote ("TAGS")))
 '(ps-line-spacing 0)
 '(ps-paragraph-spacing 0)
 '(show-paren-mode t)
 '(solarized-termcolors 256)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-separator (quote (0.5)))
 '(tool-bar-mode nil)
 '(typescript-expr-indent-offset 0)
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))

(desktop-save-mode 1)

;;(load-file "/usr/share/emacs/site-lisp/cedet/cedet-devel-load.el")
;;(semantic-load-enable-code-helpers)

;; Fix brew issues on osx
(exec-path-from-shell-initialize)

(setq ns-use-srgb-colorspace nil) ;; for nice powerline arrows

(add-to-list 'load-path "~/.emacs.d/vendor/minibuffer-fix")
(require 'minibuffer-fix)

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

(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Helm & ag
;; If async is installed
(add-to-list 'load-path "~/.emacs.d/vendor/helm")
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-async")
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-a") 'helm-projectile-ag)
(global-set-key (kbd "C-c a") (lambda () (interactive) (helm-projectile-find-file-dwim)))
(global-set-key (kbd "C-c C-f") 'helm-find-files)

;; Split window resizing
(global-set-key (kbd "<f5>") 'enlarge-window)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)

(load-file "~/.emacs.d/js-doc.el")
(require 'js-doc)
(add-hook 'js2-mode-hook
           #'(lambda ()
               (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
               (define-key js2-mode-map "@" 'js-doc-insert-tag)))

(require 'yasnippet)
(yas-global-mode t)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-reload-all)

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

;; make tslint to be run from node_modules
(defun my/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/.bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))

;(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-typescript-tslint-setup))
;(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules))

;; Smerge mode shortcut
(setq my-toggle-smerge-state nil)
(defun my-toggle-smerge ()
  (interactive)
  (if my-toggle-smerge-state
    (progn
      (smerge-mode -1)
      (flycheck-mode +1)
      (setq my-toggle-smerge-state nil)
      )
    (progn
      (smerge-mode +1)
      (flycheck-mode -1)
      (setq my-toggle-smerge-state t)
      )
    )
  )
(global-set-key (kbd "C-c m") #'my-toggle-smerge)


;; Manually set params
(global-auto-complete-mode t)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(electric-indent-mode nil)
(global-git-gutter-mode +1)
;(set-face-background 'mode-line "saddle brown")
;(set-face-foreground 'mode-line "#EBDBB2")
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-linum-mode 1)
(git-gutter:linum-setup)
(global-unset-key (kbd "C-z"))
(global-set-key "\C-x\C-z" nil)
(global-set-key (kbd "C-x C-z") nil)

(require 'git)
(require 'git-blame)

(require 'auto-indent-mode)

(setq ecb-tip-of-the-day nil)

; Ecb auto root path depending on projectile

(defvar default-ecb-source-path (list '("~/projects" "Workspace")))
(customize-set-variable 'ecb-source-path default-ecb-source-path)
;(defvar project-ecb-source-path '())
;(add-hook 'ecb-basic-buffer-sync-hook
;          (lambda ()
;            (when (functionp 'projectile-get-project-directories)
;              (when (projectile-project-p)
;                (dolist (path-dir (projectile-get-project-directories))
;                  (unless (member (list path-dir path-dir) project-ecb-source-path)
;                    (push (list path-dir path-dir) project-ecb-source-path)
;                    (customize-set-variable 'ecb-source-path project-ecb-source-path)
;                    ))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-analyse-face ((t (:inherit ecb-default-highlight-face :background "dark magenta"))))
 '(ecb-default-highlight-face ((t (:background "dark magenta"))))
 '(fic-face ((t (:background "DarkGoldenrod2" :foreground "black" :box (:line-width 2 :color "DarkGoldenrod2" :style pressed-button) :weight bold))))
 '(flycheck-error ((t (:background "red" :foreground "black"))))
 '(flycheck-fringe-error ((t (:inherit error :background "red" :foreground "black" :weight bold :width extra-expanded))))
 '(flycheck-fringe-info ((t (:background "medium sea green" :foreground "black"))))
 '(flycheck-warning ((t (:background "dark orange" :foreground "black"))))
 '(fringe ((t (:background "grey10" :weight bold :width extra-expanded))))
 '(git-gutter:added ((t (:background "forest green" :foreground "black" :weight bold))))
 '(git-gutter:modified ((t (:inherit default :background "tan3" :foreground "black" :weight bold))))
 '(highlight ((t (:background "#4e4e4e" :foreground "plum"))))
 '(js2-error ((t (:background "red" :foreground "black" :weight bold))))
 '(js2-external-variable ((t (:background "orange" :foreground "black" :weight bold))))
 '(minibuffer-prompt ((t (:background "dark slate blue" :foreground "plum"))))
 '(smerge-base ((t (:background "#404000"))))
 '(tabbar-modified ((t (:inherit tabbar-unselected :foreground "aquamarine"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "aquamarine")))))

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(define-key global-map [select] 'end-of-line)
(global-set-key (kbd "C-x f") 'find-file-in-repository)

(show-paren-mode 1)
(transient-mark-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

  (set-face-attribute 'flycheck-error nil :foreground "black" :background "red")
  (eldoc-mode +1)
  (auto-complete-mode t)
;  (turn-on-auto-fill) ;; buggy!
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
(setq tide-format-options '(
        :indentSize 2
        :tabSize 2
        :insertSpaceAfterCommaDelimiter t
        :insertSpaceAfterSemicolonInForStatements t
        :insertSpaceBeforeAndAfterBinaryOperators t
        :insertSpaceAfterKeywordsInControlFlowStatements t
        :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil
        :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets nil
        :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
        :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces nil
        :placeOpenBraceOnNewLineForFunctions nil
        :placeOpenBraceOnNewLineForControlBlocks nil
))

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
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

;; ------------------------------------------------------------------------
;; mu4e mailer setup

(add-to-list 'load-path "~/.emacs.d/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary (concat
                      "~/.emacs.d/mu4e/mu-binary-"
                      (if (eq system-type 'darwin) "mac" "linux")))

;; default
(setq mu4e-maildir "~/Mail")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; enable inline images
(setq mu4e-view-show-images t)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/INBOX" . ?i)
         ("/Sent" . ?s)
         ("/Trash" . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "~/.emacs.d/fetch.sh"
      mu4e-update-interval 900
      )

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)

(add-to-list 'mu4e-bookmarks
  '("flag:flagged"       "Important (flagged)"     ?f))

;; Defaults for new messages
(setq user-mail-address "me@ctizen.net"
      user-full-name  "Oleg Klimenko"
      mu4e-compose-signature
      (concat
        "Oleg Klimenko\n"
        "Email: " user-mail-address "\n"
        "Mobile: +7 913 725 6906\n"
        "\n"))

;; For replies we set corresponding "from:"
(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
	(setq user-mail-address
	  (cond
	    ((mu4e-message-contact-field-matches msg :to "o.klimenko@2gis.ru")
             "o.klimenko@2gis.ru")
	    ((mu4e-message-contact-field-matches msg :to "heilage.nsk@gmail.com")
             "heilage.nsk@gmail.com")
            ((mu4e-message-contact-field-matches msg :to "klimenkoov@gmail.com")
             "klimenkoov@gmail.com")
	    (t "me@ctizen.net")))))
    (setq
     user-full-name  "Oleg Klimenko"
     mu4e-compose-signature
     (concat
      "Oleg Klimenko\n"
      "Email: " user-mail-address "\n"
      "Mobile: +7 913 725 6906\n"
      "\n"))
    
    ))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'ssl
    mail-host-address "ctizen.net"
    smtpmail-default-smtp-server "smtp.zoho.com"
    smtpmail-smtp-server "smtp.zoho.com"
    smtpmail-smtp-service 465)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
