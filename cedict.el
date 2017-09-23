;;; cedict.el --- Dictionary lookup commands for CEDICT
;;
;; Copyright (C) 2017  Miles Bader
;;
;; Author: Miles Bader <miles@gnu.org>
;; Created: 23-Sep-2017

;; This file is not part of GNU Emacs.

;; This file is free software.  You can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3, or (at your option)
;; any later version.  See the file COPYING for more details.


;;; Commentary:
;;
;; Defines commands for convenient lookup of Chinese words in the
;; CEDICT Chinese-English dictionary.
;;
;; CEDICT can currently be found at:  
;;
;;    https://www.mdbg.net/chinese/dictionary?page=cc-cedict
;;
;;
;; The main user command is `cedict-lookup-at-point', which:
;;
;;  1. Finds and displays the longest matching entry in CEDICT that
;;     matches characters following point.  The displayed entry is
;;     modified to include only one term field (CEDICT entries
;;     normally contain term fields for both traditional and
;;     simplified characters).
;;
;;  2. Moves point forward by the number of matching characters.
;;     This simplifies the process of looking up consecutive words
;;     in a text.
;;
;;  3. Temporarily activates the mark at the start of the matched
;;     word, which highlights it.
;;


;;; Code:


;;; User customation

(defcustom cedict-file "/usr/local/cedict_1_0_ts_utf-8_mdbg.txt.gz"
  "CEDICT dictionary file name."
  :type 'file
  :group 'cedict)

(defcustom cedict-default-term-type 'simplified
  "Whether traditional or simplified character are preferred when presenting results.
When possible, this choice is made automatically;
this setting applies when that isn't possible."
  :type '(choice simplified traditional)
  :group 'cedict)


;;; Internal settings.

(defvar cedict-lookup-at-point-max-size 10
  "Maximum size of term searched for by `cedict-lookup-at-point'.")



;;; ----------------------------------------------------------------
;;; Functions for dealing with CEDICT entries.  
;;
;; These functions are strongly dependent on the precise format of
;; the CEDICT dictionary file, which currently consists of one line
;; per entry, where each line looks like:
;;
;; TERM_T TERM_S [PINYIN] /DEF1/DEF2/.../
;;
;; The below functions in particular rely on the following
;; properties of CEDICT entries:
;;
;;   1. Each entry is on a single line
;;
;;   2. TERM_T comes at the beginning of the line
;;   
;;   3. TERM_T and TERM_S are separated by a single space character
;;   
;;   4. Comments are indicated by "#" at the beginning of the line
;;
;; There's also some dependence on entry-ordering in the dictionary
;; file.  The file is (seemingly) ordered so that prefixes precede
;; the entries which they are prefixes of (e.g., "ab" comes before
;; "abc"), so the code here simply returns the first entry which
;; matches the longest-possible prefix of the search term.
;;


(defun cedict-entry-regexp (string)
  "Return a regexp that matches STRING as a term in a CEDICT dictionary entry."
  (concat "^\\(" string "\\|[^[/\n#]* " string "\\)"))


(defun cedict-entry-term-length (entry)
  "Return the length of the term in the CEDICT dictionary entry ENTRY."
  (let ((entry-len (length entry))
	(term-len 0))
    (while (and (< term-len entry-len)
		(/= (aref entry term-len) ?\s))
      (setq term-len (1+ term-len)))
    term-len))

(defun cedict-entry-term-prefix-length (entry prefix)
  "Return the length of a prefix of the term in the CEDICT
dictionary entry ENTRY which matches PREFIX."
  (let* ((term-len (cedict-entry-term-length entry))
	 ;; The result can at most be as long as PREFIX.
	 (max-len (min term-len (length prefix)))
	 (pfx-len 0)
	 ;; `char-equal' uses this.  It's irrelevant for Chinese
	 ;; characters, but there are a few entries for roman letters.
	 (case-fold-search t))
    ;;
    ;; CEDICT entries have two term fields, one for traditional
    ;; characters, and one for simplified, so we consider both.  
    ;;
    ;; The traditional term is the first thing on the line, and the
    ;; simplified term follows the traditional term and a space
    ;; character.
    ;;
    (while (and (< pfx-len max-len)
		(or (char-equal (aref entry pfx-len)
				(aref prefix pfx-len))
		    (char-equal (aref entry (+ term-len 1 pfx-len))
				(aref prefix pfx-len))))
      (setq pfx-len (1+ pfx-len)))
    pfx-len))


(defun cedict-entry-strip-non-matching-term (entry string default-term-to-keep)
  "Return the CEDICT dictionary entry ENTRY with the term field not matching STRING removed.
If both fields match, then DEFAULT-TERM-TO-KEEP is used to choose which term field to keep: `simplified', or `traditional'."
  (let* ((term-len (cedict-entry-term-length entry))
	 ;; The result can at most be as long as STRING.
	 (max-len (min term-len (length string)))
	 (trad-pfx-len 0)
	 (simp-pfx-len 0)
	 ;; `char-equal' uses this.  It's irrelevant for Chinese
	 ;; characters, but there are a few entries for roman letters.
	 (case-fold-search t))
    ;;
    ;; CEDICT entries have two term fields, one for traditional
    ;; characters, and one for simplified, so we consider both.  

    ;; See how much of the traditional term matches.
    ;;
    (while (and (< trad-pfx-len max-len)
		(char-equal (aref entry trad-pfx-len)
			    (aref string trad-pfx-len)))
      (setq trad-pfx-len (1+ trad-pfx-len)))

    ;; Now see how much of the simplified term matches.
    ;;
    (while (and (< simp-pfx-len max-len)
		(char-equal (aref entry (+ term-len 1 simp-pfx-len))
			    (aref string simp-pfx-len)))
      (setq simp-pfx-len (1+ simp-pfx-len)))

    ;; Now choose whichever matched more characters in STRING.
    ;;
    (if (or (> simp-pfx-len trad-pfx-len)
	    (eq default-term-to-keep 'simplified))
	;;
	;; choose simplified
	(substring entry (+ term-len 1))
      ;;
      ;; choose traditional
      (concat (substring entry 0 (+ term-len 1))
	      (substring entry (+ term-len term-len 1))))))



;;; ----------------------------------------------------------------
;;; Core search function


(defun cedict-lookup-string (string)
  "Lookup the term STRING in CEDICT, and return the longest matching entry.
If there are no entries that match any prefix of STRING, an error is signaled."
  (with-current-buffer (find-file-noselect cedict-file)
    (goto-char (point-min))
    (let* ((first-char (substring string 0 1))
	   (first-char-regexp (cedict-entry-regexp first-char))
	   ;; 99.9% of the time we'll be looking up Chinese characters
	   ;; where case-folding isn't an issue, but turn on
	   ;; case-folding anyway to properly handle the remaining
	   ;; rare cases.
	   (case-fold-search t))
      (when (not (re-search-forward first-char-regexp nil t))
	(error "No CEDICT entry found"))
      (forward-line 0)
      (let ((section-start (point)))
	(forward-line 1)
	(while (looking-at first-char-regexp)
	  (forward-line 1))
	(let ((section-end (point))
	      (result-pos section-start)
	      (search-term-length 2)
	      (failed nil))
	  (while (and (not failed) (<= search-term-length (length string)))
	    (goto-char section-start)
	    (if (not
		 (re-search-forward
		  (cedict-entry-regexp (substring string 0 search-term-length))
		  section-end
		  t))
		(setq failed t)
	      (setq result-pos (point))
	      (setq search-term-length (1+ search-term-length))))
	  (goto-char result-pos)
	  (forward-line 0)
	  (let ((result-start (point)))
	    (forward-line 1)
	    (buffer-substring-no-properties result-start (1- (point)))))))))



;;; ----------------------------------------------------------------
;;; User commands


(defun cedict-lookup-at-point (&optional both-terms)
  "Lookup the longest term following point in CEDICT, and display its entry.
Point is moved forward by the number of characters which matched.

If BOTH-TERMS is non-nil, then the CEDICT entry is displayed with
both traditional and simplified terms; normally only one of the
traditional or simplified terms in the entry is displayed."
  (interactive "P")

  ;; Skip whitespace so that point is more likely to be on a word we
  ;; can lookup.
  (skip-chars-forward "[:space:]")

  (let* ((pos (point))

	 (search-string
	  ;; Grab a nice large string to lookup.
	  ;; `cedict-lookup-string' will return the largest entry
	  ;; matching any prefix of this string, so we don't need to
	  ;; be careful about where it ends.
	  (buffer-substring-no-properties 
	   pos
	   (min (+ pos cedict-lookup-at-point-max-size) (point-max))))

	 (entry
	  ;; Do the actual dictionary lookup.
	  (cedict-lookup-string search-string))

	 (display-entry
	  ;; Unless the users says otherwise, strip off one of the
	  ;; simplified/traditional term fields, keeping whatever
	  ;; matches SEARCH-STRING.
	  (if both-terms
	      entry
	    (cedict-entry-strip-non-matching-term entry
						  search-string
						  cedict-default-term-type))))

    ;; Move the cursor past the portion of the entry that's actually
    ;; in the buffer, to make repeated invocation useful.
    (forward-char (cedict-entry-term-prefix-length entry search-string))

    ;; Move the mark so that it precedes the matched word, and fiddle
    ;; with things so that the mark is temporarily activated, allowing
    ;; it to serve as highlighting for the matched word.  The mark will
    ;; be deactivated upon any cursor movement etc.
    ;;
    ;; [This code copied from `handle-shift-selection' in Emacs
    ;; simple.el; maybe a function to this should be added?]
    (unless (and mark-active
		 (eq (car-safe transient-mark-mode) 'only))
      (setq-local transient-mark-mode
		  (cons 'only
			(unless (eq transient-mark-mode 'lambda)
			  transient-mark-mode))))
    ;; The documentation warns against using set-mark directly, but
    ;; push-mark seems to have other weird side-effects: in particular,
    ;; it suppresses the above call to forward-char...(???)
    (set-mark pos)

    ;; Display the returned dictionary entry.
    (message "%s" display-entry)))


;;; cedict.el ends here
