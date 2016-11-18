;; Tabbar
(require 'tabbar)
; turn on the tabbar
(tabbar-mode t)

(setq tabbar-mode t)
(setq tabbar-mwheel-mode t)
(setq tabbar-separator (quote (0.5)))

; define all tabs to be one of 3 possible groups: “Emacs Buffer”, “Dired”,
;“User Buffer”.
;; Tabbar settings
(set-face-attribute 'tabbar-default nil
                    :family *selected-font-family*
                    :background "gray20"
                    :foreground "gray20"
                    :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute 'tabbar-unselected nil
                    :family *selected-font-family*
                    :background "gray30"
                    :foreground "white"
                    :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute 'tabbar-selected nil
                    :family *selected-font-family*
                    :background "gray50"
                    :foreground "white"
                    :box '(:line-width 5 :color "gray50" :style nil))
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

(tabbar-mode 1)

(global-set-key [M-s-left] 'tabbar-backward)
(global-set-key [M-s-right] 'tabbar-forward)
