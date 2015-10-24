(require 'package)

;;-------------------------------------------------------------------------------------
;; Hide Startup Message
;;-------------------------------------------------------------------------------------

(setq inhibit-startup-message t)

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

;;-------------------------------------------------------------------------------------
;; Theme
;;-------------------------------------------------------------------------------------

(load-theme 'solarized-dark t)

;; 10 pt font
(set-default-font "inconsolata")
(set-face-attribute 'default nil :height 100)

(powerline-center-theme)

;;-------------------------------------------------------------------------------------
;; Line Numbering - Note: Might not look good with files with 10,000 or more lines
;;-------------------------------------------------------------------------------------

(global-linum-mode t)
(setq linum-format "%4d \u2502 ")

;;-------------------------------------------------------------------------------------
;; Ido Mode
;;-------------------------------------------------------------------------------------

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;-------------------------------------------------------------------------------------
;; Alignment
;;-------------------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;-------------------------------------------------------------------------------------
;; Flycheck
;;-------------------------------------------------------------------------------------

(add-hook 'after-init-hook #'global-flycheck-mode)

;;-------------------------------------------------------------------------------------
;; Key Bindings
;;-------------------------------------------------------------------------------------

;; Unbind C-t from transpose
(global-set-key (kbd "C-t") nil)


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


;;-------------------------------------------------------------------------------------
;; Custom
;;-------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
