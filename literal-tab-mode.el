;;; literal-tab-mode.el --- Cause TAB to insert a literal tab character
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
;; Defines the mode `literal-tab-mode'.
;;
;; When active, the TAB key inserts a literal tab character, rather
;; than performing mode-specific indentation.


;;; Code:

(defvar literal-tab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'self-insert-command)
    map)
  "Keymap for `literal-tab-mode'.")

(define-minor-mode literal-tab-mode
  "When enabled, force the TAB key to insert a literal tab character."
  :lighter " LitTab")

(define-globalized-minor-mode global-literal-tab-mode literal-tab-mode
  turn-on-literal-tab-mode)

(defun turn-on-literal-tab-mode ()
  "Unconditionally turn on literal-tab mode."
  (literal-tab-mode 1))


;;; literal-tab-mode.el ends here
