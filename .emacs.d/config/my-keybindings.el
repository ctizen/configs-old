(global-unset-key (kbd "C-z")) ;; Remove ctrl-z sleeping binding
(global-set-key "\C-x\C-z" nil) ;; Remove Ctrl-xz too
(global-set-key (kbd "C-x C-z") nil)
;; Split window resizing
(global-set-key (kbd "<f5>") 'enlarge-window)
(global-set-key (kbd "<f6>") 'enlarge-window-horizontally)
;; Often used keys
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
