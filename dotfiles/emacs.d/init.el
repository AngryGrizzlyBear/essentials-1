(require 'package)

;;--------------------------------------------------------------------------------------
;; Hide Startup Message
;;--------------------------------------------------------------------------------------

(setq inhibit-startup-message t)

;;--------------------------------------------------------------------------------------
;; Always follow symbolic links to version controlled files
;;
;; I prefer this option because I generally only get this message when I edit
;; a dotfile under version control, and repeatedly typing "yes" is annoying.
;;--------------------------------------------------------------------------------------

(setq vc-follow-symlinks t)

;;--------------------------------------------------------------------------------------
;; Small fix for keyboard internationalization problems
;;--------------------------------------------------------------------------------------

(set-keyboard-coding-system nil)

;;--------------------------------------------------------------------------------------
;; Disable Splash Screen
;;--------------------------------------------------------------------------------------

(defvar inhibit-splash-screen)
(setq inhibit-splash-screen t)

;;--------------------------------------------------------------------------------------
;; Handy editor stuff
;;--------------------------------------------------------------------------------------

(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Comment this if you dont mind git diffs complaining about missing ending newlines.
(setq require-final-newline t)

;; Uncomment this if you dont mind git diffs complaining about missing ending newlines.
; (add-hook 'before-save-hook 'delete-trailing-blank-lines)

;;--------------------------------------------------------------------------------------
;; Enable MELPA
;;--------------------------------------------------------------------------------------

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;--------------------------------------------------------------------------------------
;; Install and load Packages
;;--------------------------------------------------------------------------------------

(load-file "~/.emacs.d/package.el")

;;--------------------------------------------------------------------------------------
;; Theme
;;--------------------------------------------------------------------------------------

(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; 10 pt font
(set-frame-font "inconsolata")
(set-face-attribute 'default nil :height 100)

(powerline-center-theme)

;;--------------------------------------------------------------------------------------
;; Line Numbering - Note: Might not look good with files with 10,000 or more lines
;;--------------------------------------------------------------------------------------

(global-linum-mode t)

(defvar linum-format)
(setq linum-format "%4d \u2502 ")

;;--------------------------------------------------------------------------------------
;; Hook framemove into windmove for seamless window to frame transitions
;;--------------------------------------------------------------------------------------

(setq framemove-hook-into-windmove t)

;;--------------------------------------------------------------------------------------
;; Helm Configuration
;;--------------------------------------------------------------------------------------

(helm-mode t)
(helm-adaptive-mode t) ;; Sort results by most frequently used

(require 'helm-projectile)
(helm-projectile-on)

;; Use ag instead of grep for in-file searches in helm-find-file

(when (executable-find "ag")
  (setq helm-grep-default-command "ag -H --nogroup --nocolor %e %p %f"
        helm-grep-default-recurse-command "ag -H --nogroup --nocolor %e %p %f"))

;;-------------------------------------------------------------------------------------
;; Enable Autocomplete
;;
;; NOTE: You can replace the 't' in (setq ac-auto-start t) with a positive integer
;;       to trigger autocompletion only after that number of characters. This will
;;       improve performance on slower systems.
;;-------------------------------------------------------------------------------------

(global-auto-complete-mode t)
(setq ac-auto-start t)

;;-------------------------------------------------------------------------------------
;; ZSH script detection
;;------------------------------------------------------------------------------------

(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                                (sh-set-shell "zsh"))))

;;-------------------------------------------------------------------------------------
;; Alignment
;;-------------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;-------------------------------------------------------------------------------------
;; Flycheck
;;-------------------------------------------------------------------------------------

(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;-------------------------------------------------------------------------------------
;; Projectile Configuration
;;-------------------------------------------------------------------------------------

(projectile-global-mode)

;;-------------------------------------------------------------------------------------
;; Key Bindings
;;-------------------------------------------------------------------------------------

;; Unbind C-t from transpose

(global-set-key (kbd "C-t") nil)

(global-set-key (kbd "C-d") nil)
(global-set-key (kbd "C-m") nil)
(global-set-key (kbd "RET") nil)

;; Unbind C-c p f from projectile
(global-set-key (kbd "C-c p f") nil)

;; Neotree binds are prefixed by C-t

(global-set-key (kbd "C-t t") 'neotree-toggle)
(global-set-key (kbd "C-t n") 'neotree-create-node)
(global-set-key (kbd "C-t d") 'neotree-delete-node)
(global-set-key (kbd "C-t r") 'neotree-rename-node)
(global-set-key (kbd "C-t h") 'neotree-hidden-file-toggle)

;; Pane navigation

(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)

;; Delete surrounding

(global-set-key (kbd "C-d s") 'delete-pair)

;; Delete word

(global-set-key (kbd "C-d d") 'delete-whole-line)

;; Git utility

(global-set-key (kbd "C-m s") 'magit-status)
(global-set-key (kbd "C-m b") 'magit-blame-popup)
(global-set-key (kbd "C-m p") 'magit-dispatch-popup)
(global-set-key (kbd "C-m f") 'magit-file-popup)

;; Fix RET

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

;; Helm

(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;-------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
