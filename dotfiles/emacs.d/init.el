;;--------------------------------------------------------------------------------------
;;
;;  Emacs Configuration file
;;
;;  NOTES:
;;   - For stuff that requires privacy (such as ERC, Circe, or Email) place them in
;;     a .private.el file in your home directory. This script will load that file.
;;     This is the best way I could think of protecting sensitive data without making
;;     it incredibly inconvenient for you. Take a look at the sample private.el in
;;     the emacs.d folder (currently a work in progress).
;;
;;--------------------------------------------------------------------------------------

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
;; Conditionally load urxvt files to fix weird bindings
;;--------------------------------------------------------------------------------------

;; This code doesn't fix anything related to emacs -nw not allowing C-) in terminal
;; mode. Currently looking for solutions so I don't have to keep using C-<left>
;; and C-<right>
;;
;; (when (string= (getenv "TERM") "rxvt")
;;   (load "term/rxvt")
;;   (terminal-init-rxvt)
;;   (load "~/.emacs.d/urxvt-bindings.el")
;;   )

;;--------------------------------------------------------------------------------------
;; Enable auto-refresh to keep buffers up to date when git or another program
;; modifies them
;;--------------------------------------------------------------------------------------

(global-auto-revert-mode t)

;;--------------------------------------------------------------------------------------
;; IBuffer configurations (C-x C-b)
;;--------------------------------------------------------------------------------------

;; Filter Groups - ***** Circe filtering currently doesnt work *****

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs Configuration" (or (filename . ".emacs.d")
                                    (filename . "init.el")
                                    (filename . "package.el")
                                    (filename . "private.el")
                                    (filename . "emacs.d")))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ("Magit" (name . "\*magit"))
         ("IRC" (or (mode . "Circe*")
                    (mode . erc-mode)))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))
         ("Dired" (mode . dired-mode))
         ;; Dev has groups for all languages you program in
         ("Dev" (or  (mode . haskell-mode)
                     (mode . coffee-mode)
                     (mode . js2-mode)
                     (mode . clojure-mode)
                     (mode . cc-mode)
                     (mode . scheme-mode)
                     (mode . lisp-mode))
          )
         ("Emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")))
         ("Gnus" (or (mode . message-mode)
                     (mode . bbdb-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)
                     (name . "^\\.bbdb$")
                     (name . "^\\.newsrc-dribble")))
 	 )))

;; Automatically keep buffers up to date and load the filter
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

;;--------------------------------------------------------------------------------------
;; handy editor stuff
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

;; Add new line if using C-n navigates to the end of the buffer
(setq next-line-add-newlines t)

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
;; Load private file
;;--------------------------------------------------------------------------------------

(if (file-exists-p "~/.private.el")
  (load-file "~/.private.el")
  (setq irc-servers nil
        irc-username nil
        irc-password nil
        default-realname nil
  )
)

;;--------------------------------------------------------------------------------------
;; Configure Circe
;;--------------------------------------------------------------------------------------

(setq circe-network-options irc-servers)
(setq circe-use-cycle-completion t)
(setq helm-mode-no-completion-in-region-in-modes
      '(circe-channel-mode
        circe-query-mode
        circe-server-mode
       )
)

(setq circe-reduce-lurker-spam t)
;; (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil)) ;; Hide JOIN spam

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")))

;;--------------------------------------------------------------------------------------
;; Enable ParEdit for Emacs lisp modes
;;--------------------------------------------------------------------------------------

(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

;;--------------------------------------------------------------------------------------
;; Fill column indicator (currently set to 120 characters)
;;--------------------------------------------------------------------------------------

(require 'fill-column-indicator)

(setq fci-rule-width 1)
(setq fci-rule-color "black")
(setq fci-rule-column 120)
(add-hook 'after-change-major-mode-hook 'fci-mode)

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
;; Highlight matching parentheses when cursor is over one
;;--------------------------------------------------------------------------------------

(show-paren-mode 1)

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

;; Make sure the tab key only indents (rather than also doing completion)
(setq tab-always-indent t)

;; Make return key also indent
(electric-indent-mode 1)

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
;; Rainbow Delimiters (on most programming modes)
;;-------------------------------------------------------------------------------------

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;-------------------------------------------------------------------------------------
;; Neotree Configuration
;;-------------------------------------------------------------------------------------

(setq neo-theme 'nerd)
(setq neo-vc-integration '(char))

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

;; Unbind C-s from Isearch to make room for helm-ag
;(global-set-key (kbd "C-s") nil)

(global-set-key (kbd "C-x C-b") nil)

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

;; Kill whole line

(global-set-key (kbd "C-d d") 'kill-whole-line)

;; Git utility

(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g b") 'magit-blame-popup)
(global-set-key (kbd "C-x g p") 'magit-dispatch-popup)
(global-set-key (kbd "C-x g f") 'magit-file-popup)

;; Fix RET

(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)

;; Helm

(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;(global-set-key (kbd "C-s") 'helm-do-ag-this-file)

;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;;-------------------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))))
