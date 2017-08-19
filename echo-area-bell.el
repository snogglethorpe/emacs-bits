;;; echo-area-bell.el --- Echo-area visual bell    -*- coding: utf-8 -*-
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
;; Defines the global mode `echo-area-bell'.
;;
;; This is nice little alternative visual bell, which briefly flashes
;; the echo-area in a red color (with an optional bit of text).


;;; Code:

(defcustom echo-area-bell-string "*DING* " ;"♪"
  "Message displayed in mode-line by `echo-area-bell' function."
  :type 'string
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :type 'string
  :group 'user)

;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)

(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.

The string displayed is the value of `echo-area-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `echo-area-bell-delay'
seconds.

This function is intended to be used as a value of `ring-bell-function'."

  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
            echo-area-bell-string)
           'face '(:background "red")))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))


(define-minor-mode echo-area-bell-mode
  "Toggle echo-area visible bell."
  :global t
  (setq ring-bell-function (if echo-area-bell-mode 'echo-area-bell nil)))


;;; echo-area-bell.el ends here
