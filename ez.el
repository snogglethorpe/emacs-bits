;;; ez.el --- Ez [Gosmacs-like] compatibility package for GNU emacs
;;
;; Copyright (C) 1988,2019  Miles Bader
;;
;; Author: Miles Bader <miles@gnu.org>
;; Created: 10-Jan-1988
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software.  You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.  See the file COPYING for more details.


;;; Code:

(provide 'ez)

(defun ez-split-string (string sep-re)
  (let ((result nil))
    (while (string-match sep-re string)
      (setq result (cons (substring string 0 (match-beginning 0)) result))
      (setq string (substring string (match-end 0))))
    (nreverse (cons string result))))
  
(defun ez-fast-filter-region (program start end)
  (interactive "sCommand: \nd\nm")
  (if (< end start)
      (let ((temp start))
	(setq start end)
	(setq end temp)))
  (let ((argv (ez-split-string program "[ \t]+")))
    (apply 'call-process-region start end (car argv) t t nil (cdr argv))))
  
(defun ez-filter-region (program start end)
  (interactive "sCommand: \nd\nm")
  (if (< end start)
      (let ((temp start))
	(setq start end)
	(setq end temp)))
  (call-process-region start end "/bin/sh" t t nil "-c" program))
  
(defun ez-source-region (program start end)
  (interactive "sCommand: \nd\nm")
  (if (< end start)
      (let ((temp start))
	(setq start end)
	(setq end temp)))
  (call-process-region start end "/bin/sh" nil nil nil "-c" program))

(defun ez-top-o-window ()
  "Move the point to the top line of this window"
  (interactive)
  (move-to-window-line 0))
(defun ez-bottom-o-window ()
  "Move the point to the bottom line of this window"
  (interactive)
  (move-to-window-line -1))

(defun ez-kill-current-buffer ()
  "Kills the current buffer"
  (interactive)
  (kill-region (point-min) (point-max)))

;;; these visit- functions are just intended to change prompts around
(defun ez-visit-file (file)
  "Visits FILE; if a copy is already in buffer, the buffer is visited
instead"
  (interactive "FVisit file: ")
  (find-file file))
(defun ez-visit-file-other-frame (filename)
  "Edit file FILENAME, in another frame.
May create a new frame, or reuse an existing one.
See the function `display-buffer'."
  (interactive "FVisit file in other frame: ")
  (switch-to-buffer-other-frame (find-file-noselect filename)))
(defun ez-visit-file-other-window (filename)
  "Edit file FILENAME, in another window.
May create a new window, or reuse an existing one.
See the function `display-buffer'."
  (interactive "FVisit file in other window: ")
  (switch-to-buffer-other-window (find-file-noselect filename)))

(defun ez-visit-buffer (buf)
  "Visits BUFFER, creating it if it doesn't exist"
  (interactive "BVisit buffer: ")
  (switch-to-buffer buf))
(defun ez-visit-buffer-other-window (buf)
  "Visits BUFFER in another window, creating it if it doesn't exist"
  (interactive "BVisit buffer in other window: ")
  (switch-to-buffer-other-window buf))
(defun ez-visit-buffer-other-frame (buf)
  "Visits BUFFER in another frame, creating it if it doesn't exist"
  (interactive "BVisit buffer in other frame: ")
  (switch-to-buffer-other-frame buf))

(defun ez-read-file (filename)
  "Replaces the current buffer with a buffer on FILENAME"
  (interactive
   (list (read-file-name "Read file: " (buffer-file-name (current-buffer)) nil t)))
  (and (or (not (buffer-modified-p))
	   (yes-or-no-p "Buffer modified; read new file anyway? "))
       (let ((point (point)))
	 (set-buffer-modified-p nil)	; so kill-buffer won't ask
	 (kill-buffer (current-buffer))
	 (find-file filename)
	 (goto-char point))))

(defun ez-write-file (filename)
  "Write the current buffer out to FILENAME, subsequently visiting it."
  (interactive
   (list (read-file-name "Write file: " (buffer-file-name (current-buffer)))))
  (write-file filename))

(defun ez-twiddle-following-char (n)
  "Reverses the case of the following N characters and moves past them"
  (interactive "p")
  (let ((pos (point))
	(step (if (< n 0) -1 1))
	(fixup (if (< n 0) -1 0)))
    (while (/= n 0)
      (setq n (- n step))
      (let ((c (char-after (+ pos fixup))))
	(cond ((and (> c 64) (< c 91))
	       (delete-char step)
	       (insert-char (+ c 32) 1))
	      ((and (> c 96) (< c 123))
	       (delete-char step)
	       (insert-char (- c 32) 1))
	      (t
	       (forward-char (- step fixup)))))
      (forward-char fixup)
      (setq pos (+ pos step)))))

(defun ez-transpose-previous-chars ()
  "Tranposes the two characters BEFORE the point"
  (interactive)
  (backward-char 1)
  (transpose-chars nil))

(defun ez-save-all-buffers ()
  "Saves all modified buffers, without asking"
  (interactive)
  (save-some-buffers t))

(defun ez-iota (n)
  (let ((result nil))
    (while (> n 0)
      (setq n (1- n))
      (setq result (cons n result)))
    result))

(defun ez-swap-backspace-delete ()
  "Swaps the backspace and delete keys in the translation table"
  (interactive)

  (if (null keyboard-translate-table)
      (setq keyboard-translate-table (concat (ez-iota 128))))
  
  (let ((obs (aref keyboard-translate-table ?\b))
	(odl (aref keyboard-translate-table ?\177)))
    (aset keyboard-translate-table ?\b odl)
    (aset keyboard-translate-table ?\177 obs)))

(defun ez-switch-to-old-buffer (b)
  "Switches to another buffer only if it exists"
  (interactive "bBuffer")
  (switch-to-buffer (get-buffer b)))

(defun ez-previous-window ()
  "Moves the cursor to the previous window"
  (interactive)
  (other-window -1))

(defun ez-delete-window-or-bury-buffer ()
  (interactive)
  (if (one-window-p)
      (bury-buffer)
    (delete-window)))

(defun ez-line-to-top-of-window ()
  "Moves line top of the current window"
  (interactive)
  (recenter 0))

(defun ez-hack-sexp-region (fun count)
  (save-excursion
    (let ((syn (char-syntax (following-char))))
      (if (or (eq syn ?_) (eq syn ?w) (eq syn ?\() (eq syn ?\") (eq syn ?\'))
	  (forward-sexp 1)))
    (let ((start (point)))
      (backward-sexp count)
      (funcall fun (point) start))))

(defun ez-upcase-prev-sexp (p)
  "Convert the current sexp, or previous sexp if the point is between the sexps,
and the ARG-1 previous sexps to upper case."
  (interactive "p")
  (ez-hack-sexp-region (function upcase-region) p))

(defun ez-capitalize-prev-sexp (p)
  "Capitalize the current sexp, or previous sexp if the point is between the
sexps, and the ARG-1 previous sexps."
  (interactive "p")
  (ez-hack-sexp-region (function capitalize-region) p))

(defun ez-downcase-prev-sexp (p)
  "Convert the current sexp, or previous sexp if the point is between the sexps,
and the ARG-1 previous sexps to lower case."
  (interactive "p")
  (ez-hack-sexp-region (function downcase-region) p))

(define-key minibuffer-local-map "\C-u" 'ez-kill-current-buffer)
(define-key minibuffer-local-completion-map "\C-u" 'ez-kill-current-buffer)
(if (boundp 'minibuffer-local-ns-map)
    (define-key minibuffer-local-ns-map "\C-u" 'ez-kill-current-buffer))
(define-key minibuffer-local-must-match-map "\C-u" 'ez-kill-current-buffer)

(autoload 'ispell-word "ispell" nil t)

(fset 'filter-region 'ez-filter-region)
(fset 'fast-filter-region 'ez-fast-filter-region)

(global-set-key "\M-$" 'ispell-word)
(global-set-key "\C-^" 'ez-twiddle-following-char)
(global-set-key "\C-t" 'ez-transpose-previous-chars)
(global-set-key "\C-l" 'redraw-display)
(global-set-key "\C-z" 'ez-scroll-one-line-up)
(global-set-key "\C-q" 'ez-scroll-one-line-down)
(global-set-key "\M-Y" (function (lambda (n) (interactive "p") (yank-pop (- n)))))
(global-set-key "\M-q" 'query-replace)
(global-set-key "\M-Q" 'query-replace-regexp)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-R" 'replace-regexp)
(global-set-key "\M-u" 'ez-upcase-prev-sexp)
(global-set-key "\M-l" 'ez-downcase-prev-sexp)
(global-set-key "\M-c" 'ez-capitalize-prev-sexp)
(global-set-key "\M-\C-r" 'isearch-backward-regexp)
(global-set-key "\M-g" 'fill-paragraph)
(global-set-key "\M-%" 'toggle-read-only)
(global-set-key "\M-!" 'ez-line-to-top-of-window)
(global-set-key "\M-n" 'goto-line)
(global-set-key "\M-N" 'what-line)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key "\M-," 'ez-top-o-window)
(global-set-key "\M-." 'ez-bottom-o-window)
(global-set-key "\M-/" 'move-to-window-line)
(global-set-key "\C-x\C-u" 'undo)
(global-set-key "\C-x\C-e" 'compile)
(global-set-key "\C-x\C-k" 'kill-compilation)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key "\C-x\C-p" 'previous-error)
(global-set-key "\C-xn" 'other-window)
(global-set-key "\C-xp" 'ez-previous-window)
(global-set-key "\C-xd" 'ez-delete-window-or-bury-buffer)
(global-set-key "\C-x5d" 'delete-frame)
(global-set-key "\C-x\C-v" 'ez-visit-file)
(global-set-key "\C-x4\C-v" 'ez-visit-file-other-window)
(global-set-key "\C-x5\C-v" 'ez-visit-file-other-frame)
(global-set-key "\C-xb" 'ez-visit-buffer)
(global-set-key "\C-x4b" 'ez-visit-buffer-other-window)
(global-set-key "\C-x5b" 'ez-visit-buffer-other-frame)
(global-set-key "\C-x\C-r" 'ez-read-file)
(global-set-key "\C-x\C-w" 'ez-write-file)
(global-set-key "\C-x\C-m" 'ez-save-all-buffers)
(global-set-key "\C-x\C-q" 'quoted-insert)
(global-set-key "\C-x\C-i" 'insert-file)
(global-set-key "\C-xi" 'indent-rigidly)
(global-set-key "\C-x\C-o" 'switch-to-old-buffer)

;; Because we rebind M-/, steal another key for dabbrev-expand, which
;; is a very useful function.
;;
(global-set-key "\M-'" 'dabbrev-expand)


;;; ---------------------------------------------------------------
;;; the following stuff stolen from William Lott's .emacs

(defun ez-scroll-one-line-down (lines)
  "Scrolls the current window down LINES, or one if no arg."
  (interactive "p")
  (scroll-down lines))

(defun ez-scroll-one-line-up (lines)
  "Scrolls the current window up LINES, or one if no arg."
  (interactive "p")
  (scroll-up lines))

; Tweak the completion handling. Still not quite like I want it but...
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)
(if (boundp 'minibuffer-local-ns-map)
    (define-key minibuffer-local-ns-map " " 'minibuffer-complete))
(define-key minibuffer-local-must-match-map " " 'minibuffer-complete)

; I don't like the completion code doing things because it thought I wanted
; them done. If I want help I will type '?', when I want to answer I will hit
; return.
(setq minibuffer-completion-confirm t)
(setq completion-auto-help nil)
