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

(defconst cedict-lookup-at-point-max-size 10
  "Maximum size of term searched for by `cedict-lookup-at-point'.")

(defconst cedict-dubious-variant-definition-regexp
  "/\\(?:old\\|japanese\\) variant of "
  "A regexp which matches the definition portion of 'variant' entries.
We typically want to ignore such entries when searching for a
simplified character.")



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
;;   5. The pinyin section follows the terms, and is surrounded by spaces
;;      and square brackets.
;;
;; There's also some dependence on entry-ordering in the dictionary
;; file.  The file is (seemingly) ordered so that prefixes precede
;; the entries which they are prefixes of (e.g., "ab" comes before
;; "abc"), so the code here simply returns the first entry which
;; matches the longest-possible prefix of the search term.
;;


(defun cedict-entry-regexp (string)
  "Return a regexp matching entries for terms starting with STRING in a CEDICT dictionary.
The extent of the regexp is guaranteed to end at the start of the
entry's definition section (so, for instance, searching forward
for that regexp will leave point on the definition)."
  (concat "^\\(?:" string "\\|[^[/\n#]* " string "\\)[^[/\n]]*[^]]*] "))


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


(defun cedict-low-quality-definition-p ()
  "Return non-nil if the definition following point is 'low-quality'.
Point must actually be at the start of the _definition_ portion
of an entry (not at the beginning of the entry).

'Low-quality' entries are ignored when another entry for the same term exists.

Currently 'low-quality' means only /surname .../ entries."
  (looking-at "/surname "))



;;; ----------------------------------------------------------------
;;; Core search functions


(defun cedict-search-for-string (string)
  "Search for the term STRING in the current buffer, which should in CEDICT format.
Set point to the beginning of the first matching entry.
If there are no entries that match any prefix of STRING, an error is signaled."
  (goto-char (point-min))
  (let* ((first-char (substring string 0 1))
	 (first-char-regexp (cedict-entry-regexp first-char))
	 ;; 99.9% of the time we'll be looking up Chinese characters
	 ;; where case-folding isn't an issue, but turn on
	 ;; case-folding anyway to properly handle the remaining
	 ;; rare cases.
	 (case-fold-search t))

    ;; Find the first entry in the dictionary beginning with the first
    ;; character of STRING.
    ;;
    ;; We have to be a little picky in case of /old variant .../ and
    ;; /japanese variant .../ entries, in which case we only want to
    ;; match when STRING starts with the _traditional_ character (the
    ;; variant).  Because such entries are usually not contiguous with
    ;; the main block of entries for the simplified character, stopping
    ;; here on a simplified character match could cause us to miss the
    ;; real definition.  The `cedict-dubious-variant-definition-regexp'
    ;; regexp matches the precise set of defintions we want to treat
    ;; this way.
    ;;
    (let ((keep-looking t))
      (while keep-looking
	;; Search for an initial entry matching the first character.
	;;
	(when (not (re-search-forward first-char-regexp nil t))
	  (error "No CEDICT entry found"))

	;; Stop looking, unless, as per the comment above, we're looking
	;; at a "dubious variant" entry.  In the latter case, only stop
	;; here when STRING's first character matches the traditional
	;; term (which is the first thing in the entry).
	;;
	(unless (and (looking-at cedict-dubious-variant-definition-regexp)
		     (not (= (aref string 0)
			     (char-after (line-beginning-position)))))
	  (setq keep-looking nil))))

    (let (;; Location of successful match
	  (result-pos (point))
	  ;; Length of the term we're looking for
	  (search-term-length 1))

      ;; Increase the size of the prefix of STRING we're looking for,
      ;; unless our initial search found a "low-quality" definition.
      ;;
      ;; Although in most cases multiple definitions for a term are
      ;; grouped into a single entry, CEDICT occasionally has multiple
      ;; entries for the same term.
      ;;
      ;; By default our search process will return the first entry in
      ;; such cases, so if we're looking at a low-quality definition,
      ;; we'd like to continue searching further with this same
      ;; prefix length, in case we can find something better.
      ;;
      (unless (cedict-low-quality-definition-p)
	(setq search-term-length (1+ search-term-length)))

      ;; Our initial search from the beginning of the file, for a
      ;; single-character prefix of STRING, put us on the first entry
      ;; starting with that characters.  Entries in the dictionary are sorted
      ;; by the first character, so we can easily locate the founds of the
      ;; section beginning with that character, which allows us to constrain
      ;; subsequent searches for longer prefixes of STRING in a much smaller
      ;; region.
      ;;
      (forward-line 0)			; move back to start of first entry
      (let ((section-start (point)))

	;; Skip until we find an entry not starting with the same first
	;; character.
	;;
	(forward-line 1)
	(while (looking-at first-char-regexp)
	  (forward-line 1))

	(let ((section-end (point))
	      (failed nil))

	  ;; In case we're going to start out by searching for another
	  ;; single-character entry (which can happen when the initial
	  ;; entry was "low-quality"), move to a place where the search
	  ;; won't simply find the initial entry again.
	  ;;
	  (goto-char result-pos)

	  (while (and (not failed) (<= search-term-length (length string)))
	    (if (not
		 (re-search-forward
		  (cedict-entry-regexp (substring string 0 search-term-length))
		  section-end
		  t))
		;; Stop the search loop
		(setq failed t)

	      ;; Remember the last place we successfully found something.
	      (setq result-pos (point))

	      ;; Increase the size of the prefix of STRING we're looking for,
	      ;; unless this is a "low-quality" definition.
	      ;;
	      (unless (cedict-low-quality-definition-p)
		(setq search-term-length (1+ search-term-length)))))

	  ;; Move to the beginning of the last successful match.
	  ;;
	  (goto-char result-pos)
	  (forward-line 0))))))


(defun cedict-lookup-string (string)
  "Lookup the term STRING in CEDICT, and return the longest matching entry.
If there are no entries that match any prefix of STRING, an error is signaled."
  (with-current-buffer (find-file-noselect cedict-file)
    (cedict-search-for-string string)
    (let ((result-start (point)))
      (forward-line 1)
      (buffer-substring-no-properties result-start (1- (point))))))



;;; ----------------------------------------------------------------
;;; Helper functions


(defun cedict-search-string-at-point ()
  "Return a 'reasonably large' search string starting at point.
'Reasonably large' means `cedict-lookup-at-point-max-size' characters long
(because the actual lookup commands will return partial prefix matches, there's no need to be especially careful about the length of the search term)."
  (buffer-substring-no-properties
   (point)
   (min (+ (point) cedict-lookup-at-point-max-size) (point-max))))



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

  (let* ((start-pos (point))

	 (search-string (cedict-search-string-at-point))

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
    (set-mark start-pos)

    ;; Display the returned dictionary entry.
    (message "%s" display-entry)))


;;; cedict.el ends here
