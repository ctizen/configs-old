;; Dart

(add-to-list 'load-path "~/.emacs.d/dart-mode-custom/")
(add-to-list 'load-path "~/.emacs.d/dart-mode-custom/company-dart/")
(require 'dart-mode)
(require 'company-yankpad)
(require 'helm-dart)
(require 'company-dart)

(add-hook 'dart-mode-hook 'flycheck-mode)
(add-hook 'dart-mode-hook (lambda ()
 (set (make-local-variable 'company-backends)
  '(company-dart (company-dabbrev company-yankpad)))))

(setq company-minimum-prefix-length 0)
(add-hook 'dart-mode-hook
          #'(lambda ()
              (define-key dart-mode-map (kbd "C-c C-d") 'dart-hover-information)
              (define-key dart-mode-map (kbd "C-c C-h") 'dart-type-hierarchy)))






;(setq dart-enable-analysis-server t)

;(defvar last-post-command-position 0
;  "Holds the cursor position from the last run of post-command-hooks.")

;(make-variable-buffer-local 'last-post-command-position)

;(defun dart-show-hover-command ()
;  ""
;  (unless (or (equal (point) last-post-command-position) (bound-and-true-p dart-mode))
;    (let ((my-current-word (thing-at-point 'word)))
;      (dart-show-hover)))
;  (setq last-post-command-position (point)))



;(defun setup-dart-mode ()
;  "Setup dart."
;  (interactive)
;  (flycheck-mode +1)
;  (company-mode +1)
;  (define-key dart-mode-map (kbd "M-.") 'dart-goto)
;  (define-key dart-mode-map (kbd "M-SPC") 'dart-show-hover)
;;  (add-to-list 'post-command-hook #'dart-show-hover-command)
;  )
;(add-hook 'dart-mode-hook 'setup-dart-mode)

;; Enable plantuml-mode for PlantUML files
;(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))
;(setq plantuml-jar-path "/usr/local/share/plantuml/plantuml.jar")
;(setq plantuml-output-type "png")


