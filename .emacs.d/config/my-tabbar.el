;; Tabbar
(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)

(setq tabbar-mode t)
(setq tabbar-mwheel-mode t)
(setq tabbar-separator (quote (0.5)))
(setq tabbar-background-color "black")
(setq tabbar-use-images nil)

; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.
;; Tabbar settings
(set-face-attribute 'tabbar-default nil
                    :family *selected-font-family*
                    :background "black"
                    :foreground "black"
                    :box '(:line-width 1 :color "black" :style nil))
(set-face-attribute 'tabbar-unselected nil
                    :family *selected-font-family*
                    :background "black"
                    :foreground "white"
                    :box '(:line-width 5 :color "black" :style nil))
(set-face-attribute 'tabbar-selected nil
                    :family *selected-font-family*
                    :background "gray30"
                    :foreground "white"
                    :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute 'tabbar-highlight nil
                    :family *selected-font-family*
                    :background "purple"
                    :foreground "white"
                    :underline nil
                    :box '(:line-width 5 :color "purple" :style nil))
(set-face-attribute 'tabbar-button nil
                    :family *selected-font-family*
                    :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'tabbar-separator nil
                    :family *selected-font-family*
                    :background "gray20"
                    :height 0.6)
(set-face-attribute 'tabbar-modified nil
                    :inherit 'tabbar-unselected
                    :foreground "aquamarine")
(set-face-attribute 'tabbar-selected-modified nil
                    :inherit 'tabbar-selected
                    :foreground "aquamarine")

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

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
  There are two groups: Emacs buffers (those whose name starts with '*', plus
  dired buffers), and the rest.  This works at least with Emacs v24.2 using
  tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

