;; Packages management
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config/")
(load-library "my-recent")
(load-library "my-colorthemes")
(load-library "my-fonts")
(load-library "my-helm-and-ag")
(load-library "my-js-mode")
(load-library "my-smerge")
(load-library "my-tide")
(load-library "my-web-mode")
(load-library "my-tabbar")
(load-library "my-mailer")
(load-library "my-flycheck")
(load-library "my-projectile")
(load-library "my-keybindings")
(load-library "my-misc-packages")
(load-library "my-misc-settings")
(load-library "my-ecb")
(load-library "my-git")


;; Customs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3C3836" "#FB4934" "#84BB26" "#FABD2F" "#83A598" "#D3869B" "#3FD7E5" "#EBDBB2"])
 '(custom-safe-themes
   (quote
    ("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "0eea76fe89061a7f6da195f4a976c0b91150de987b942fac2dd10992aea33833" "5a0930a84612f861bb5e98999a50ec6ef7995676c7330aac9b8deda1aaa45f83" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "345f8f92edc3508574c61850b98a2e0a7a3f5ba3bb9ed03a50f6e41546fe2de0" default)))
 '(ecb-layout-name "left14")
 '(ecb-minor-mode-text "")
 '(ecb-options-version "2.50")
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-height 0.15)
 '(ecb-windows-width 0.2)
 '(package-selected-packages
   (quote
    (alect-themes afternoon-theme color-theme-sanityinc-solarized mew popwin web-mode tabbar rich-minority restclient markdown-preview-mode magit-popup js3-mode js2-mode js-doc iedit helm-projectile helm-ag git-gutter git-commit git-blame git flymake-php find-file-in-repository fic-mode exec-path-from-shell ecb darktooth-theme ctags-update color-theme-solarized color-theme-modern calfw auto-indent-mode ag))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((t (:background "dark magenta")))))
