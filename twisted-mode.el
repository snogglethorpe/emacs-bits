;;; twisted-mode.el --- Insert characters in "twisted" form    -*- coding: utf-8 -*-
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
;; Defines the mode `twisted-mode'.
;;
;; When active:
;;
;;  1. Self-inserting characters are inserted in "twisted" form, which
;;     basically means "upside down and reversed."
;
;;  2. The direction of insertion is reversed, so that subsquent
;;     characters are inserted before, rather than after, the
;;     characters which preceded them.
;;
;; The result is that you can type upside-down and reversed text.
;;
;; The set of characters for which a twisted form is known is defined
;; in the variable `twisted-mapping'.  Characters without a known
;; twisted form are inserted as-is, but the insertion direction is
;; still reversed.


;;; Code:

(defcustom twisted-mapping
  '((?a . ?ɐ) (?b . ?q) (?c . ?ɔ) (?d . ?p)
    (?e . ?ǝ)           (?g . ?ᵷ) (?h . ?ɥ)
    (?i . ?ᴉ)           (?k . ?ʞ) ;(?l . ?ꞁ)
    (?m . ?ɯ) (?n . ?u)            (?p . ?d)
    (?q . ?b) (?r . ?ɹ)           (?t . ?ʇ)
    (?u . ?n) (?v . ?ʌ) (?w . ?ʍ)
    (?y . ?ʎ)
    (?, . ?‘) (?' . ?‚)
    (?. . ?˙) (?? . ?¿) (?! . ?¡)
    (?\( . ?\)) (?\) . ?\() (?\[ . ?\]) (?\] . ?\[)
    (?< . ?>) (?> . ?<)
    (?“ . ?„)
    )
  "Mapping from normal characters to twisted characters used by `self-insert-twisted'."
  :type '(alist :key-type character :value-type character)
  :group 'text)


(defvar twisted-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'self-insert-twisted)
    (define-key map [remap delete-backward-char] 'delete-char)
    (define-key map [remap backward-delete-char] 'delete-char)
    (define-key map [remap backward-delete-char-untabify] 'delete-char)
    map)
  "Keymap for `twisted-mode'.")


(define-minor-mode twisted-mode
  "When enabled, self-inserting characters are inserted in \"twisted\" form.
The direction of insertion is also reversed.

See the variable `twisted-mapping' for the mapping from normal to
twisted form.  Characters without a known twisted form are
inserted verbatim."
  :lighter " pǝʇsᴉʍT")


(defun self-insert-twisted (arg)
  "Like `self-insert-command', but try to insert a twisted variant.
The mapping from normal character to twisted characters is taken
from `twisted-mapping'."
  (interactive "p")
  (setq last-command-event
	(or (cdr (assq last-command-event twisted-mapping))
	    last-command-event))
  (self-insert-command arg)
  (backward-char arg))


;;; twisted-mode.el ends here
