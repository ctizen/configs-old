;; Dart

(add-to-list 'load-path "~/.emacs.d/dart-mode-custom/")
(require 'dart-mode)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-hook 'dart-mode-hook 'flycheck-mode)
(add-hook 'dart-mode-hook 'company-mode)


