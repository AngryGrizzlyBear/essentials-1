(require 'cl)
 
(defvar my-packages '(ag auto-complete clojure-mode coffee-mode css-mode framemove flycheck
                         flycheck-clojure flycheck-haskell flycheck-rust
                         flycheck-pos-tip flx-ido gitignore-mode haskell-mode js2-mode
                         less-css-mode magit neotree paredit scss-mode powerline projectile
                         rust-mode color-theme-solarized wrap-region)
  "Packages to make sure are installed")
 
(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))
 
(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
