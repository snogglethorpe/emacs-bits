;;; visible-whitespace-mode.el --- Make whitespace visible    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2001,2005,2009,2017  Miles Bader
;;
;; Author: Miles Bader <miles@gnu.org>
;; Created: 26-Nov-2001
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software.  You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.  See the file COPYING for more details.


;;; Commentary:
;;
;; Defines the mode `visible-whitespace-mode'.
;;
;; When active, normally invisible whitespace characters are made
;; visible.
;;
;; This is different from typical "highlight whitespace" packages in
;; that each whitespace character is indicated individually, even when
;; they're adjacent to each other, which makes it easy to see otherwise
;; hard to find whitespace problems.


;;; Code:

(defface visible-whitespace
  '((((background dark)) :foreground "grey40" :background "grey10" :bold t)
    (((background light)) :foreground "grey70" :background "grey90" :bold t))
  "Face for control-characters revealed by `visible-whitespace-mode'."
  :group 'whitespace)

(defcustom visible-whitespace-mappings
  '((?\n [?¶ ?\n] [?$ ?\n])
    ;; Note that TAB still tabs, but with a graphic indicator before the
    ;; tab; we only use single-character graphic indicator to reduce the
    ;; number of cases where the indicator causes the tabbing to be
    ;; screwed up.
    (?\t [?» ?\t] [?\\ ?\t])
    (?   [?·] [?.]))
  "An alist of mappings for displaying whitespace in `visible-whitespace-mode'.

The car of each mapping is a whitespace character, and the cdr is a list of
display vectors (a vector of characters).  The first display vector the
list that can be displayed is used; if no display vector for a mapping can
be displayed, then that character is displayed unmodified.

The characters in are displayed using the `visible-whitespace' face."
  :type 'list
  :group 'whitespace)


(defun visible-whitespace-mode-legal-display-vector-p (vec)
  "Return true if every character in the display vector VEC can be displayed."
  (let ((i 0) (len (length vec)))
    (while (and (< i len) (char-displayable-p (aref vec i)))
      (setq i (1+ i)))
    (= i len)))

;; Buffer local variable used to remember whether a buffer initially had
;; a local display table or not.
(defvar visible-whitespace-mode-display-table-was-local nil)


(define-minor-mode visible-whitespace-mode
  "Toggle Visible Whitespace mode
When active, normally invisible whitespace characters are made visible.

With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :lighter " VisWS"
  (if visible-whitespace-mode
      (progn
	(set (make-local-variable 
	      'visible-whitespace-mode-display-table-was-local)
	     buffer-display-table)
	(unless buffer-display-table
	  (setq buffer-display-table (make-display-table)))
	(dolist (entry visible-whitespace-mappings)
	  (let ((vecs (cdr entry)))
	    (while (and vecs
			(not
			 (visible-whitespace-mode-legal-display-vector-p
			  (car vecs))))
	      (setq vecs (cdr vecs)))
	    (when vecs
	      (let ((vec (copy-sequence (car vecs))))
		(dotimes (i (length vec))
		  (when (not (eq (aref vec i) (car entry)))
		    (aset vec i
			  (make-glyph-code (aref vec i) 'visible-whitespace))))
		(aset buffer-display-table (car entry) vec))))))
    (if visible-whitespace-mode-display-table-was-local
	(dolist (entry visible-whitespace-mappings)
	  (aset buffer-display-table (car entry) nil))
      (setq buffer-display-table nil))))


;;; visible-whitespace-mode.el ends here
