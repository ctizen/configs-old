(load-file "~/.emacs.d/vendor/minibuffer-fix/minibuffer-fix.el") ;; against minibuffer bug

(desktop-save-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(define-key global-map [select] 'end-of-line)
(exec-path-from-shell-initialize) ;; Fix brew issues on osx

;; Tmp files settings
(setq temporary-file-directory "~/.emacs.d/tmp")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq ns-use-srgb-colorspace nil) ;; for nice powerline arrows
(setq ring-bell-function 'ignore) ;; dont beep
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq fringe-mode '(14 . nil))
(setq line-spacing 0.2)
(setq fci-rule-color "#d6d6d6")
(setq show-paren-mode t)
(setq solarized-termcolors 256)

;; Tooltip colors
(setq pos-tip-background-color "color-23")
(setq pos-tip-foreground-color "color-230")

(global-auto-complete-mode t)
(global-auto-revert-mode t)
(global-git-gutter-mode +1)
(global-linum-mode 1)
(git-gutter:linum-setup)
(electric-indent-mode nil)

(setq tramp-default-method "ssh")
(setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side

(set-face-attribute 'fringe nil
                    :background "grey10"
                    :weight 'bold
                    :width 'extra-expanded)
(set-face-attribute 'highlight nil
                    :background "#4e4e4e"
                    :foreground "plum")
(set-face-attribute 'minibuffer-prompt nil
                    :background "dark magenta"
                    :foreground "plum")
