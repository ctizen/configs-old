;; Vcs
(require 'git)
(require 'git-blame)

(setq git-gutter:modified-sign "%")
(setq git-gutter:update-interval 2)
(setq global-git-gutter-mode t)

(set-face-attribute 'git-gutter:added nil
                    :background "forest green"
                    :foreground "black"
                    :weight 'bold)
(set-face-attribute 'git-gutter:modified nil
                    :inherit 'default
                    :background "tan3"
                    :foreground "black"
                    :weight 'bold)

