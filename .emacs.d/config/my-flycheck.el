(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook
          (lambda ()
            (flycheck-add-mode 'javascript-eslint 'js2-mode)))

(set-face-attribute 'flycheck-error nil
		    :background "red"
		    :foreground "black")
(set-face-attribute 'flycheck-fringe-error nil
		    :inherit 'error
		    :background "red"
		    :foreground "black"
		    :weight 'bold
		    :width 'extra-expanded)
(set-face-attribute 'flycheck-fringe-info nil
		    :background "medium sea green"
		    :foreground "black")
(set-face-attribute 'flycheck-warning nil
		    :background "dark orange"
		    :foreground "black")

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
