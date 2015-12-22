(require 'cl)

(defvar my-packages '(ag auto-complete clojure-mode coffee-mode css-mode circe framemove flycheck
                         esh-help fill-column-indicator flycheck-clojure flycheck-haskell flycheck-rust
                         flycheck-pos-tip gitignore-mode golden-ratio haskell-mode swiper linum-relative
                         js2-mode less-css-mode magit neotree paredit scala-mode2 scss-mode smex powerline
                         projectile rust-mode color-theme-solarized wrap-region stylus-mode rainbow-delimiters)
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
