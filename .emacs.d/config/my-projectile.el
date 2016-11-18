;; Projectile project manager
(projectile-global-mode)
(setq projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules" "vendor" "bin" "assets")))
(setq projectile-globally-ignored-file-suffixes nil)
(setq projectile-globally-ignored-files (quote ("TAGS")))

(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))
