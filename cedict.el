;;; cedict.el --- Dictionary lookup commands for CEDICT -*- lexical-binding: t -*-
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
;; The main user command is `cedict-lookup-longest-at-point' which:
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

;;
;; This code uses a secondary data structure for lookups, as doing
;; simple text-search based lookups is too inefficient.
;;
;; The reasons for this is are:
;;
;;   1. Because Chinese does not use word separators, we want to be able
;;      to do "longest prefix" lookups, meaning we can't just search for
;;      a single term and declare success or failure.
;;
;;   2. There may be multiple entries for a given search term, which may
;;      not be adjacent in the file.
;;
;;   3. CEDICT dictionary files are not sorted in a useful way (they
;;      actually are sorted but each entry has two keys which may have
;;      different sort orders).
;;
;;   4. Related entries aren't always contiguous.
;;
;;   5. Because of points (2) - (4), to properly do "longest prefix"
;;      lookups would require doing a full-buffer search (search
;;      starting at the beginning of the buffer) for each character in a
;;      lookup term, and CEDICT dictionary files are big enough that
;;      this can be pretty slow, and this cost would be incurred for
;;      *every lookup*.
;;
;; However, because CEDICT dictionaries can be very large, building the
;; secondary data structure for the entire dictonary would be very slow.
;;
;; To get around these issues, what we do is build the secondary data
;; structure incrementally, adding entries for every term beginning with
;; a given character the first time such a term is looked up.  This
;; requires a scan through the entire buffer, but only once for a given
;; prefix character.  Subsequent lookups for terms with the same prefix
;; character will use the secondary data structure instead, which is
;; very fast.
;;
;; The result is that lookups are generally fast, but slightly slower
;; the first time any term beginning with a given character is looked
;; up.  However even these initial lookups are acceptably fast for
;; interactive use.
;;
;; The secondary data structure used is a hash table mapping prefix
;; characters to a lookup trie for all terms starting with that
;; character.  The leaves in the trie contain buffer positions of
;; entries in the CEDICT dictionary buffer.
;;


;;; Code:


;;; User customation

(defcustom cedict-file "/usr/local/cedict_1_0_ts_utf-8_mdbg.txt.gz"
  "CEDICT dictionary file name."
  :type 'file
  :group 'cedict)

(defface cedict-lookup-highlight
  '((t :inherit highlight))
  "Face for highlighting search term used by `cedict-lookup-at-point'."
  :group 'whitespace)


;;; Internal settings.

(defconst cedict-suppressed-definition-regexp
  ;; Almost all "surname" entries in CEDICT are pretty useless, as the
  ;; given name matches the Pinyin portion of the entry exactly.  As
  ;; so many characters in Chinese are valid surnames, these end up
  ;; just being noise.
  "/surname [^/]*/$"
  "A regexp which matches the definition of CEDICT entries to be ignored.")

(defconst cedict-traditional-only-definition-regexp
  ;; CEDICT "variant" entries are a mixed bag and aren't alway
  ;; consistent, but the main problematic entires we want to suppress
  ;; are those where an old variant of a newer character actually
  ;; includes the newer character as its simplified term, which are
  ;; wrong if looked up via the simplified character (which should
  ;; never return these entries).  Almost all single-character variant
  ;; entries basically apply only to the traditional term, so we only
  ;; add them via that term.  Multi-character variant entries often
  ;; refer to variant _phrasing_, and so have both valid traditional
  ;; and simplified terms.
  "/[^/]*variant of .[\[|/]"
  "A regexp which matches the definition of entries for which we only use traditional term.
The simplified term in such entries is ignored.")



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


(defun cedict-entry-regexp (string)
  "Return a regexp matching entries for terms starting with STRING in a CEDICT dictionary.
The extent of the regexp is guaranteed to end at the start of the
entry's definition section (so, for instance, searching forward
for that regexp will leave point on the definition)."
  (concat "^\\(?:" string "\\|[^[/\n#]* " string "\\)[^[/\n]]*[^]]*] "))



;;; ----------------------------------------------------------------
;;; CEDICT lookup trie

;; CEDICT lookup-trie format:
;;
;;  TRIE:	(ENTRIES . TAILS)
;;  ENTRIES:	(ENTRY-POS ...)			; list of buffer positions
;;  TAILS:	((CHAR . TAIL-TRIE) ...)	; association list of tail tries
;;  CHAR:	unicode character
;;  ENTRY-POS:	position in dictionary buffer
;;


(defsubst cedict-make-trie ()
  "Return a new empty CEDICT lookup trie."
  (cons nil nil))

(defsubst cedict-trie-is-leaf-p (trie)
  "Return non-nil if the CEDICT lookup trie TRIE has entries."
  (car trie))

(defsubst cedict-trie-next (trie next-char)
  "Return a new CEDICT lookup trie, which appends NEXT-CHAR to TRIE.
Return nil if there are no entries beginning with NEXT-CHAR."
  (cdr (assq next-char (cdr trie))))

(defsubst cedict-trie-add-next (trie next-char)
  "Return a new CEDICT lookup trie, which appends NEXT-CHAR to TRIE.
If there are no existing entries beginning with NEXT-CHAR, a new
empty trie is added to TRIE, and returned."
  ;; Get the assoc cell for NEXT-CHAR
  (let ((next-entry (assq next-char (cdr trie))))
    ;; If there's none, add one
    (when (null next-entry)
      (setq next-entry (cons next-char (cons nil nil)))
      (setcdr trie (cons next-entry (cdr trie))))
    ;; ... and return the associated trie
    (cdr next-entry)))

(defsubst cedict-trie-entries (trie)
  "Return the list of entries for the CEDICT lookup trie TRIE.
The entry list is a list of buffer positions in the CEDICT
dictionary buffer, each being the beginning of one entry."
  (car trie))

(defsubst cedict-trie-add-entry (trie entry-pos)
  "Add the entry buffer position ENTRY-POS to TRIE.
If ENTRY-POS already exists in TRIE, nothing is done.
ENTRY-POS is added after any existing entries."
  (unless (memq entry-pos (car trie))
    ;; We're always going to add ENTRY-POS as a list, so just make it
    ;; one here.
    (setq entry-pos (list entry-pos))
    (let ((tail (car trie)))
      (if (null tail)
	  ;; TRIE has no existing entries, so just add the first one.
	  (setcar trie entry-pos)
	;; Find the last cons-cell in TRIE's entry position list, so
	;; we can extend it.
	(while (cdr tail)
	  (setq tail (cdr tail)))
	(setcdr tail entry-pos)))))



;;; ----------------------------------------------------------------
;;; CEDICT dictionary

;; CEDICT dictionary format:
;;
;;  DICTIONARY:	hash-table<CHAR => TRIE>
;;


(defun cedict-make-dict ()
  "Return a new CEDICT dictionary structure.
This is an internal data structure used to hold cached CEDICT
dictionary entries."
  (make-hash-table :test 'eq))

(defsubst cedict-dict-lookup-char (dict char)
  "Lookup CHAR in the CEDICT dictionary structure DICT, returning its trie.
If there are no entries beginning with CHAR, return nil."
  (gethash char dict))


(defun cedict-dict-add-term (dictionary entry-pos)
  "Add the entry at ENTRY-POS, for the term beginning at point, to DICTIONARY.
The term added begins at point, and extends until the next space.
DICTIONARY is a CEDICT dictionary data structure."
  (let* ((first-char (char-after))
	 (trie (gethash first-char dictionary)))

    ;; If DICTIONARY has no entries beginning with FIRST-CHAR, add the
    ;; initial data-structure to hold them.
    ;;
    (when (null trie)
      (setq trie (cedict-make-trie))
      (puthash first-char trie dictionary))

    ;; Skip the first character as we're done with it, it's only used as
    ;; the dictionary key.
    ;;
    (forward-char 1)

    ;; Now scan the remaining characters in the term, building up the
    ;; trie as necessary.
    ;;
    (let ((next-char (char-after)))
      (while (and next-char (/= next-char ?\s))
	(setq trie (cedict-trie-add-next trie next-char))
	(forward-char 1)
	(setq next-char (char-after))))

    ;; There are no more characters left in this term, so TRIE is a
    ;; leaf.  Add ENTRY-POS to it.
    ;;
    (cedict-trie-add-entry trie entry-pos)))


(defun cedict-dict-add-entries (dictionary first-char)
  "Add entries for terms beginning with the character FIRST-CHAR to DICTIONARY.
All entries in the current buffer, which should be a
CEDICT-format dictionary file, are added.
DICTIONARY is a CEDICT dictionary data structure."
  (let ((first-char-regexp (cedict-entry-regexp (char-to-string first-char)))
	;; 99.9% of the time we'll be looking up Chinese characters
	;; where case-folding isn't an issue, but turn on
	;; case-folding anyway to properly handle the remaining
	;; rare cases.
	(case-fold-search t))

    ;; Scan through the entire buffer looking for FIRST-CHAR-REGEXP, and
    ;; add each line we find to DICTIONARY.
    ;;
    (goto-char (point-min))
    (while (re-search-forward first-char-regexp nil t)
      (unless (looking-at cedict-suppressed-definition-regexp)
	(let ((traditional-only
	       ;; true if we should only add this entry for the traditional term
	       (looking-at cedict-traditional-only-definition-regexp)))

	  (forward-line 0)
	  (let ((entry-pos (point)))

	    ;; Add an entry for the traditional term.
	    (cedict-dict-add-term dictionary entry-pos)

	    ;; Skip the separating space and add an entry for simplified term.
	    (unless traditional-only
	      (forward-char 1)
	      (cedict-dict-add-term dictionary entry-pos))

	    (forward-line 1)))))))



;;; ----------------------------------------------------------------
;;; "Lookup state" functions, which hold state during term lookup.

;; Lookup state format:
;;
;; LOOKUP-STATE:	(CURRENT-TRIE . DICT-BUF)
;;


(defvar cedict-dictionary nil
  "Dictionary data structure for currently known CEDICT entries.
This is buffer-local, and the CEDICT lookup code normally uses
the definition in the buffer holding the dictionary text.")
(make-variable-buffer-local 'cedict-dictionary)


(defsubst cedict-lookup-state-is-leaf-p (lookup-state)
  "Return true if LOOKUP-STATE has dictionary entries."
  (cedict-trie-is-leaf-p (car lookup-state)))

(defun cedict-lookup-state-entries (lookup-state)
  "Return a list of the CEDICT entry buffer positions in LOOKUP-STATE."
  (cedict-trie-entries (car lookup-state)))

(defsubst cedict-lookup-state-buffer (lookup-state)
  "Return the buffer containing the CEDICT dictionary for LOOKUP-STATE."
  (cdr lookup-state))


(defsubst cedict-lookup-state-next (lookup-state next-char)
  "Return a new lookup state which appends NEXT-CHAR to LOOKUP-STATE."
  (let ((next-trie (cedict-trie-next (car lookup-state) next-char)))
    (and next-trie
	 (cons next-trie
	       (cdr lookup-state)))))


(defun cedict-make-lookup-state (dict-buf first-char)
  "Return a new lookup state for the CEDICT dictionary in the buffer DICT-BUF."
  (with-current-buffer dict-buf
    (save-excursion
      (when (null cedict-dictionary)
	(setq cedict-dictionary (cedict-make-dict)))
      (let ((dict cedict-dictionary))
	(cons (or (cedict-dict-lookup-char dict first-char)
		  (progn
		    (cedict-dict-add-entries dict first-char)
		    (cedict-dict-lookup-char dict first-char)))
	      dict-buf)))))


(defun cedict-lookup-state-definition-text (lookup-state &optional term)
  "Returns a string containing all the definitions for LOOKUP-STATE."
  (with-current-buffer (cdr lookup-state)
    (save-excursion
      (let ((text nil))
	(dolist (entry-pos (cedict-trie-entries (car lookup-state)))
	  (goto-char entry-pos)
	  (when term
	    (skip-chars-forward "^["))
	  (let ((entry-text
		 (buffer-substring-no-properties (point) (line-end-position))))
	    (when term
	      (setq entry-text (concat term " " entry-text)))
	    (setq text (if text (concat text "\n" entry-text) entry-text))))
	text))))



;;; ----------------------------------------------------------------
;;; General Lookup functions


(defun cedict-lookup (term)
  "Lookup TERM in CEDICT and return the final lookup state.

If no entry is found for TERM, return nil.

The lookup state can be turned into a string using
`cedict-lookup-state-definition-text'."
  (let ((lookup-state
	 (cedict-make-lookup-state (find-file-noselect cedict-file)
				   (aref term 0)))
	(tpos 1))

    ;; Scan through TERM, descending through the dictionary lookup trie as we go.
    ;;
    (while (and lookup-state (< tpos (length term)))
      (setq lookup-state
	    (cedict-lookup-state-next lookup-state (aref term tpos)))
      (setq tpos (1+ tpos)))

    lookup-state))



;;; ----------------------------------------------------------------
;;; User commands


(defvar cedict-lookup-term-highlight-overlay
  (let ((ov (make-overlay 1 1)))
    (overlay-put ov 'face 'cedict-lookup-highlight)
    ov)
  "Overlay used by `cedict-lookup-longest-at-point' to highlight search term.")


(defun cedict-lookup-longest-at-point (&optional both-terms)
  "Lookup the longest term following point in CEDICT, and display its entry.

Furthermore, point is moved forward to the end of the longest
term found, and the term itself is temporarily highlighted in the
buffer.

If BOTH-TERMS is non-nil, then the CEDICT entry is displayed with
both traditional and simplified terms; normally only one of the
traditional or simplified terms in the entry is displayed.

If there are multiple matching entries, they are all displayed."
  (interactive "P")

  ;; Skip whitespace so that point is more likely to be on a word we
  ;; can lookup.
  (skip-chars-forward "[:space:][:punct:]")
  (when (eobp)
    (error "No search term found"))

  (let ((lookup-state
	 (cedict-make-lookup-state (find-file-noselect cedict-file)
				   (char-after)))
	(longest-lookup-state nil)
	(start-pos (point)))

    (while lookup-state
      (when (cedict-lookup-state-is-leaf-p lookup-state)
	(setq longest-lookup-state lookup-state))
      (forward-char 1)
      (setq lookup-state (cedict-lookup-state-next lookup-state (char-after))))

    (if longest-lookup-state
	(let ((definition
		(cedict-lookup-state-definition-text
		 longest-lookup-state
		 (and (not both-terms)
		      (buffer-substring-no-properties start-pos (point))))))

	  ;; Temporarily highlight the term we found.  As we lookup a
	  ;; variable-length term, and move the cursor, this can help
	  ;; the user keep track of what happened.
	  ;;
	  (move-overlay cedict-lookup-term-highlight-overlay
			start-pos (point) (current-buffer))

	  ;; Display the returned dictionary entry.
	  (message "%s" definition)

	  (sit-for 5)
	  (delete-overlay cedict-lookup-term-highlight-overlay))

      (error "No CEDICT entries found beginning with \"%c\""
	     (char-after start-pos)))))


(defun cedict-lookup-string (term &optional both-terms)
  "Lookup TERM in CEDICT, and display its entry.

If there are multiple matching entries, they are all displayed.

If BOTH-TERMS is non-nil, then the CEDICT entry is displayed with
both traditional and simplified terms; normally only one of the
traditional or simplified terms in the entry is displayed."
  (interactive "MLookup Chinese term: \nP")

  ;; Remove leading/trailing whitespace and punctuation from TERM.
  (setq term (string-trim term "[[:space:][:punct:]]+" "[[:space:][:punct:]]+"))

  (let ((lookup-state (cedict-lookup term)))

    ;; Display a textual definition unless there was an error.
    ;;
    (if lookup-state
	(message "%s"
		 (cedict-lookup-state-definition-text
		  lookup-state
		  (and (not both-terms) term)))
      (error "No CEDICT entries found for \"%s\"" term))))


(defun cedict-lookup-region (&optional start end both-terms)
  "Lookup the string between START and END in CEDICT, and display its entry.

If there are multiple matching entries, they are all displayed.

If BOTH-TERMS is non-nil, then the CEDICT entry is displayed with
both traditional and simplified terms; normally only one of the
traditional or simplified terms in the entry is displayed."
  (interactive "r")
  (cedict-lookup-string (buffer-substring-no-properties start end) both-terms))


;;; cedict.el ends here
