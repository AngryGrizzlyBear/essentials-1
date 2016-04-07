(require 'cl)

(defvar my-packages '(ag auto-complete clojure-mode coffee-mode css-mode circe expand-region framemove flycheck
                         esh-help enh-ruby-mode ess flymake-ruby ruby-block robe inf-ruby ac-inf-ruby rspec-mode
                         fill-column-indicator flycheck-clojure flycheck-haskell flycheck-rust slim-mode
                         flycheck-pos-tip gitignore-mode golden-ratio haskell-mode swiper sass-mode linum-relative
                         js2-mode less-css-mode magit neotree paredit scala-mode2 scss-mode smex powerline toml-mode
                         projectile rust-mode color-theme-solarized wrap-region stylus-mode rainbow-delimiters
                         livescript-mode jade-mode)
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
