;; This file adds the missing escape sequences for urxvt so emacs -nw
;; will work with Ctrl+Shift, Ctrl+Meta, etc sequences.
;;
;; Credit for the original code goes to Gilles on emacs.stackexchange.com
;;

(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))
(defun eval-after-load-urxvt ()
  (when (and (boundp 'rxvt-alternatives-map) (boundp 'rxvt-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key rxvt-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)

                ("\e\[%d;3~" meta)
                ("\e\[%d;5~" control)
                ("\e\[%d;6~" control shift)
                ("\e\[%d;7~" control meta)
                ("\e\[%d;8~" control meta shift)))
        (setq c (1+ c))))))
(eval-after-load "rxvt" '(eval-after-load-urxvt))
