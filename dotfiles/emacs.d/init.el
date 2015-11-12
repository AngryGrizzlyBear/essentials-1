(require 'package)

;;-------------------------------------------------------------------------------------
;; Hide Startup Message
;;-------------------------------------------------------------------------------------

(setq inhibit-startup-message t)

;;-------------------------------------------------------------------------------------
;; Disable Splash Screen
;;-------------------------------------------------------------------------------------

(defvar inhibit-splash-screen)
(setq inhibit-splash-screen t)

;;-------------------------------------------------------------------------------------
;; Enable MELPA
;;-------------------------------------------------------------------------------------

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;-------------------------------------------------------------------------------------
;; Install and load Packages
;;-------------------------------------------------------------------------------------

(load-file "~/.emacs.d/package.el")

;; Load flx after its installed
(require 'flx-ido)

;;-------------------------------------------------------------------------------------
;; Theme
;;-------------------------------------------------------------------------------------

(load-theme 'solarized t)

;; 10 pt font
(set-frame-font "inconsolata")
(set-face-attribute 'default nil :height 100)

(powerline-center-theme)

;;-------------------------------------------------------------------------------------
;; Line Numbering - Note: Might not look good with files with 10,000 or more lines
;;-------------------------------------------------------------------------------------

(global-linum-mode t)

(defvar linum-format)
(setq linum-format "%4d \u2502 ")

;;-------------------------------------------------------------------------------------
;; Ido Mode
;;-------------------------------------------------------------------------------------


(defvar ido-enable-flex-matching)
(defvar ido-everywhere)
(defvar ido-use-faces)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-faces nil)
(flx-ido-mode 1)
(ido-mode 1)

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
