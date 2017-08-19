;;; caps-lock-mode.el --- Cause TAB to insert a literal tab character
;;
;; Copyright (C) 2008,2017  Miles Bader
;;
;; Author: Miles Bader <miles@gnu.org>
;; Created: 21-Oct-2008
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software.  You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.  See the file COPYING for more details.


;;; Commentary:
;;
;; Defines the mode `caps-lock-mode'.
;;
;; When active, self-inserting characters are inserted in uppercase
;; (as defined by the `upcase' function).


;;; Code:

(defvar caps-lock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'self-insert-upcased)
    map)
  "Keymap for `caps-lock-mode'.")

(define-minor-mode caps-lock-mode
  "When enabled, convert all self-inserting characters to uppercase."
  :lighter " CapsLock")

(defun self-insert-upcased (arg)
  "Like `self-insert-command', but insert the character upcased."
  (interactive "p")
  (setq last-command-event (upcase last-command-event))
  (self-insert-command arg))


;;; caps-lock-mode.el ends here
