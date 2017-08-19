;;; list-fonts-display.el --- List system fonts nicely
;;
;; Copyright (C) 2008,2017  Miles Bader
;;
;; Author: Miles Bader <miles@gnu.org>
;; Created: 21-Oct-2008
;; Keywords: faces

;; This file is not part of GNU Emacs.

;; This file is free software.  You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.  See the file COPYING for more details.


;;; Commentary:
;;
;; Defines the command `list-fonts-display'.
;;
;; This is a command for displaying, in a nice way, the list of system
;; fonts (by default, as output by the font-config "fc-list" command.


;;; Code:

(defcustom list-fonts-display-command "fc-list : family"
  "Shell command run to output a font list for `list-fonts-display'.
The output should contain one font name per line."
  :type 'string
  :group 'font-selection)


(defun list-fonts-display (&optional matching)
  "Display a list of font-families available via font-config, in a new buffer.

If the optional argument MATCHING is non-nil, only font families
matching that regexp are displayed; interactively, a prefix
argument will prompt for the regexp.

The name of each font family is displayed using that family, as
well as in the default font (to handle the case where a font
cannot be used to display its own name)."
  (interactive
   (list
    (and current-prefix-arg
	 (read-string "Display font families matching regexp: "))))
  (let (families)
    (with-temp-buffer
      (shell-command list-fonts-display-command t)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((fam (buffer-substring (line-beginning-position)
				     (line-end-position))))
	  (when (or (null matching) (string-match matching fam))
	    (push fam families)))
	(forward-line)))
    (setq families
	  (sort families
		(lambda (x y) (string-lessp (downcase x) (downcase y)))))
    (let ((buf (get-buffer-create "*Font Families*")))
      (with-current-buffer buf
	(erase-buffer)
	(dolist (family families)
	  ;; We need to pick one of the comma-separated names to
	  ;; actually use the font; choose the longest one because some
	  ;; fonts have ambiguous general names as well as specific
	  ;; ones.
	  (let ((family-name
		 (car (sort (split-string family ",")
			    (lambda (x y) (> (length x) (length y))))))
		(nice-family (replace-regexp-in-string "," ", " family)))
	    (insert (concat (propertize nice-family
					'face (list :family family-name))
			    " (" nice-family ")"))
	    (newline)))
	(goto-char (point-min)))
      (display-buffer buf))))


;;; list-fonts-display.el ends here
