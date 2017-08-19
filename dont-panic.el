;;; dont-panic.el --- At Emacs startup, display "Don't Panic" in large, friendly, letters
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


;;; Code:

;; Redefine the Emacs startup message to be "Don't Panic" in large,
;; friendly, letters.
;;
(setq initial-scratch-message
      (propertize "Don't\nPanic\n"
		  'font-lock-face '(:height 10.0 :inherit variable-pitch))
      inhibit-startup-screen t)
