(require 'cl)
 
(defvar my-packages '(ag clojure-mode coffee-mode css-mode flycheck
                         flycheck-clojure flycheck-haskell flycheck-rust
                         flycheck-pos-tip flx-ido gitignore-mode haskell-mode js2-mode
                         less-css-mode neotree paredit scss-mode powerline projectile
                         rust-mode solarized-theme wrap-region)
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
