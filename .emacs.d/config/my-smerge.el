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
;(set-face-attribute 'smerge-base nil :background "#404000")
