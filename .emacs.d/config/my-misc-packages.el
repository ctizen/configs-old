;; Popup window manager
(require 'popwin)
(popwin-mode 1)

;; Browser
(defvar *my-browser* (if (eq system-type 'darwin)
                         'browse-url-default-macosx-browser
                         'browse-url-chromium))
(setq browse-url-browser-function *my-browser*)

;; Buffer selection tool
(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))

;; Todos and fixmes
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(set-face-attribute 'fic-face nil
                    :background "DarkGoldenrod2"
                    :foreground "black"
                    :box '(:line-width 2 :color "DarkGoldenrod2" :style 'pressed-button)
                    :weight 'bold)

;; Snippets
(require 'yasnippet)
(yas-global-mode t)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-reload-all)

;; Folding with Ctrl-Enter
(require 'yafolding)
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
(add-hook 'after-init-hook
          (lambda () (yafolding-mode)))

;; My notes
(defun open-notepad (filename)
  (interactive "sFile name:> ")
  (find-file (concat "/notepad@furiten.ru#2022:~/notes/" filename)))
