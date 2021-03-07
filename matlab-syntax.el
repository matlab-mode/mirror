;;; matlab-syntax.el --- Manage MATLAB syntax tables and buffer parsing.
;;
;; Copyright (C) 2021 Eric Ludlam
;;
;; Author:  <eludlam@mathworks.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Manage syntax handling for `matlab-mode'.
;; Matlab's syntax for comments and strings can't be handled by a standard
;; Emacs syntax table.  This code handles the syntax table, and special
;; scanning needed to augment a buffer's syntax for all our special cases.
;;
;; This file also handles all the special parsing needed to support indentation,
;; block scanning, and the line.

(require 'matlab-compat)

;;; Code:

(defvar matlab-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Comment Handling:
    ;;   Multiline comments:   %{ text %}
    ;;   Single line comments: % text (single char start)
    ;;   Ellipsis omments:     ... text (comment char is 1st char after 3rd dot)
    ;;                            ^ handled in `matlab--syntax-propertize'
    (modify-syntax-entry ?%  "< 13" st)
    (modify-syntax-entry ?{  "(} 2c" st)
    (modify-syntax-entry ?}  "){ 4c" st)
    (modify-syntax-entry ?\n ">"    st)

    ;; String Handling:
    ;;   Character vector:    'text'
    ;;   String:              "text"
    ;;       These next syntaxes are handled with `matlab--syntax-propertize'
    ;;   Transpose:           varname'
    ;;   Quoted quotes:       ' don''t '    or " this "" "
    ;;   Unterminated Char V: ' text 
    (modify-syntax-entry ?'  "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    
    ;; Words and Symbols:
    (modify-syntax-entry ?_  "_" st)

    ;; Punctuation:
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?/  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)

    ;; Parentheticl blocks:
    ;;   Note: these are in standard syntax table, repeated here for completeness.
    (modify-syntax-entry ?\(  "()" st)
    (modify-syntax-entry ?\)  ")(" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    ;;(modify-syntax-entry ?{  "(}" st) - Handled as part of comments
    ;;(modify-syntax-entry ?}  "){" st)
    
    st)
  "MATLAB syntax table")

(defvar matlab-navigation-syntax-table
  (let ((st (copy-syntax-table matlab-syntax-table)))
    ;; Make _ a part of words so we can skip them better
    (modify-syntax-entry ?_  "w" st)
    st)
  "The syntax table used when navigating blocks.")

(defmacro matlab-navigation-syntax (&rest forms)
  "Set the current environment for syntax-navigation and execute FORMS."
  (declare (indent 0))
  (list 'let '((oldsyntax (syntax-table))
	       (case-fold-search nil))
	(list 'unwind-protect
	      (list 'progn
		    '(set-syntax-table matlab-navigation-syntax-table)
		    (cons 'progn forms))
	      '(set-syntax-table oldsyntax))))

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec matlab-navigation-syntax def-body)))

;;; Buffer Scanning for Syntax Table Augmentation
;;
;; To support all our special syntaxes via syntax-ppss (parse partial
;; sexp), we need to scan the buffer for patterns, and then leave
;; behind the hints pps needs to do the right thing.
;;
;; Support is broken up in these functions:
;;    * matlab--put-char-category - Apply a syntax category to a character
;;    * matlab--syntax-symbol     - Create a syntax category symbol
;;    * matlab--syntax-propertize - Used as `syntax-propertize-function' for
;;                                  doing the buffer scan to augment syntxes.
;;    * matlab--scan-line-*       - Scan for specific types of syntax occurances.

(defun matlab--put-char-category (pos category)
  "At character POS, put text CATEGORY."
  (put-text-property pos (1+ pos) 'category category)
  (put-text-property pos (1+ pos) 'mcm t)
  )

(defmacro matlab--syntax-symbol (symbol syntax doc)
  "Create a new SYMBOL used as a text property category with SYNTAX."
  (declare (indent defun))
  `(progn (defvar ,symbol ,syntax ,doc)
	  (set ',symbol ,syntax) ;; So you can re-eval it.
	  (put ',symbol 'syntax-table ,symbol)
	  ))

(matlab--syntax-symbol matlab--unterminated-string-syntax '(15 . nil) ;; Generic string end
  "Syntax placed on end-of-line for unterminated strings.")
(put 'matlab--unterminated-string-syntax 'unterminated t) ;; Font-lock cookie

(matlab--syntax-symbol matlab--ellipsis-syntax (string-to-syntax "< ") ;; comment char
  "Syntax placed on ellipsis to treat them as comments.")

(matlab--syntax-symbol matlab--not-block-comment-syntax (string-to-syntax "(}") ;; Just a regular open brace
  "Syntax placed on ellipsis to treat them as comments.")

(defun matlab--syntax-propertize (&optional start end)
  "Scan region between START and END for unterminated strings.
Only scans whole-lines, as MATLAB is a line-based language.
If region is not specified, scan the whole buffer.
See `matlab--scan-line-for-ellipsis', `matlab--san-line-bad-blockcomment',
and `matlab--scan-line-for-unterminated-string' for specific details."
  (save-match-data ;; avoid 'Syntax Checking transmuted the match-data'
    (save-excursion
      ;; Scan region, but always expand to beginning of line
      (goto-char (or start (point-min)))
      (beginning-of-line)
      ;; Clear old properties
      (remove-text-properties (point) (save-excursion (goto-char (or end (point-max)))
						      (end-of-line) (point))
			      '(category nil mcm nil))
      ;; Apply properties
      (while (and (not (>= (point) (or end (point-max)))) (not (eobp)))

	;; Multiple ellipsis can be on a line.  Find them all
	(beginning-of-line)
	(while (matlab--scan-line-for-ellipsis)
	  ;; Mark ellipsis as if a comment.
	  (matlab--put-char-category (point) 'matlab--ellipsis-syntax)
	  (forward-char 3)
	  )

	;; Multiple invalid block comment starts possible.  Find them all
	(beginning-of-line)
	(while (matlab--scan-line-bad-blockcomment)
	  ;; Mark 2nd char as just open brace, not punctuation.
	  (matlab--put-char-category (point) 'matlab--not-block-comment-syntax)
	  )

	;; Look for an unterminated string.  Only one possible per line.
	(beginning-of-line)
	(when (matlab--scan-line-for-unterminated-string)
	  ;; Mark this one char plus EOL as end of string.
	  (let ((start (point)))
	    (matlab--put-char-category (point) 'matlab--unterminated-string-syntax)
	    (end-of-line)
	    (matlab--put-char-category (point) 'matlab--unterminated-string-syntax)
	    ))

	(beginning-of-line)
	(forward-line 1))
      )))

(matlab--syntax-symbol matlab--transpose-syntax '(3 . nil) ;; 3 = symbol
  "Treat ' as non-string when used as transpose.")

(matlab--syntax-symbol matlab--quoted-string-syntax '(9 . nil) ;; 9 = escape in a string
  "Treat '' or \"\" as not string delimeteres when inside a string.")

(defun matlab--scan-line-for-unterminated-string (&optional debug)
  "Scan this line for an unterminated string, leave cursor on starting string char."
  ;; First, scan over all the string chars.
  (save-restriction
    (narrow-to-region (point-at-bol) (point-at-eol))
    (beginning-of-line)
    (condition-case err
	(while (re-search-forward "\\s\"\\|\\s<" nil t)
	  (let ((start-str (match-string 0))
		(start-char (match-beginning 0)))
	    (forward-char -1)
	    (if (looking-at "\\s<")
		(forward-comment 1)
	      ;; Else, check for valid string
	      (if (or (bolp)
		      (string= start-str "\"")
		      (save-excursion
			(forward-char -1)
			(not (looking-at "\\(\\w\\|\\s_\\|\\s)\\|\"\\|\\.\\)"))))
		  (progn
		    ;; Valid string start, try to skip the string
		    (forward-sexp 1)
		    ;; If we just finished and we have a double of ourselves,
		    ;; convert those doubles into punctuation.
		    (when (looking-at start-str)
		      (forward-char -1)
		      (matlab--put-char-category (point) 'matlab--quoted-string-syntax)
		      ;; and try again.
		      (goto-char start-char)
		      ))
		(when (string= start-str "'")
		  ;; If it isn't valid string, it's just transpose or something.
		  ;; convert to a symbol - as a VAR'', the second ' needs to think it
		  ;; is not after punctuation.
		  (matlab--put-char-category (point) 'matlab--transpose-syntax))
		;; Move forward 1.
		(forward-char 1)
		)))
	  nil)
      (error
       t))))

(defun matlab--scan-line-bad-blockcomment ()
  "Scan this line for invalid block comment starts."
  (save-restriction
    (narrow-to-region (point-at-bol) (point-at-eol))
    (when (and (re-search-forward "%{" nil t) (not (looking-at "\\s-*$")))
      (goto-char (1- (match-end 0)))
      t)))

(defun matlab--scan-line-for-ellipsis ()
  "Scan this line for an ellipsis."
  (save-restriction
    (narrow-to-region (point-at-bol) (point-at-eol))
    (when (re-search-forward "\\.\\.\\." nil t)
      (goto-char (match-beginning 0))
      t)))

;;; Font Lock Support:
;;
;; The syntax specific font-lock support handles comments and strings.
;;
;; We'd like to support multiple kinds of strings and comments.  To do
;; that we overload `font-lock-syntactic-face-function' with our own.
;; This does the same job as the orriginal, except we scan the start
;; for special cookies left behind by `matlab--syntax-propertize' and
;; use that to choose different fonts.
(defun matlab--font-lock-syntactic-face (pps)
  "Return the face to use for the syntax specified in PPS."
  ;; From the default in font-lock.
  ;; (if (nth 3 state) font-lock-string-face font-lock-comment-face)
  (if (nth 3 pps)
      ;; This is a string.  Check the start char to see if it was
      ;; marked as an unterminate string.
      (if (get-text-property (nth 8 pps) 'unterminated)
	  matlab-unterminated-string-face
	font-lock-string-face)
    font-lock-comment-face)
  )

;;;
;;; EXPERIMENTAL INTEGRATION:
;;;
;;
;; Connect our special logic into a running MATLAB Mode
;; replacing existing mechanics.
;;
;; Delete this if/when it becomes a permanent part of `matlab-mode'.

(defun matlab-syntax-setup ()
  "Integrate our syntax handling into a running `matlab-mode' buffer.
Safe to use in `matlab-mode-hook'."
  ;; Syntax Table support
  (set-syntax-table matlab-syntax-table)
  (make-local-variable 'syntax-propertize-function)
  (setq syntax-propertize-function 'matlab--syntax-propertize)
  ;; Comment handlers
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'page-delimiter)
  (setq comment-start "%"
	comment-end   ""
        comment-start-skip "%\\s-+"
	page-delimiter "^\\(\f\\|%%\\(\\s-\\|\n\\)\\)")
  ;; Other special regexps handling different kinds of syntax.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^\\(\f\\|%%\\(\\s-\\|\n\\)\\)")
  
  ;; Font lock
  (make-local-variable 'font-lock-syntactic-face-function)
  (setq font-lock-syntactic-face-function 'matlab--font-lock-syntactic-face)
  )

;;; Syntax Testing for Strings and Comments
;;
;; These functions detect syntactic context based on the syntax table.
(defun matlab-cursor-in-string-or-comment ()
  "Return non-nil if the cursor is in a valid MATLAB comment or string."
  (let* ((pps (syntax-ppss (point))))
    (nth 8 pps)))

(defun matlab-cursor-in-comment ()
  "Return t if the cursor is in a valid MATLAB comment."
  (let* ((pps (syntax-ppss (point))))
    (nth 4 pps)))

(defun matlab-cursor-in-string (&optional incomplete)
  "Return t if the cursor is in a valid MATLAB character vector or string scalar.
Note: INCOMPLETE is now obsolete
If the optional argument INCOMPLETE is non-nil, then return t if we
are in what could be a an incomplete string. (Note: this is also the default)"
  (let* ((pps (syntax-ppss (point))))
    (nth 3 pps)))

(defun matlab-cursor-comment-string-context (&optional bounds-sym)
  "Return the comment/string context of cursor for the current line.
Return 'comment if in a comment.
Return 'string if in a string.
Return 'charvector if in a character vector
Return 'ellipsis if after an ... ellipsis
Return nil if none of the above.
Scans from the beginning of line to determine the context.
If optional BOUNDS-SYM is specified, set that symbol value to the
bounds of the string or comment the cursor is in"
  (let* ((pps (syntax-ppss (point)))
	 (start (nth 8 pps))
	 (end 0)
	 (syntax nil))
    ;; Else, inside something if 'start' is set.
    (when start
      (save-match-data
	(save-excursion
	  (goto-char start) ;; Prep for extra checks.
	  (setq syntax
		(cond ((eq (nth 3 pps) t)
		       (if (= (following-char) ?')
			   'charvector
			 'string))
		      ((eq (nth 3 pps) ?')
		       'charvector)
		      ((eq (nth 3 pps) ?\")
		       'string)
		      ((nth 4 pps)
		       (if (= (following-char) ?\%)
			   'comment
			 'ellipsis))
		      (t nil)))
	  
	  ;; compute the bounds
	  (when (and syntax bounds-sym)
	    (if (memq syntax '(charvector string))
		;;(forward-sexp 1) - overridden - need primitive version
		(goto-char (scan-sexps (point) 1))
	      (forward-comment 1)
	      (if (bolp) (forward-char -1)))
	    (set bounds-sym (list start (point))))
	  )))

    ;; Return the syntax
    syntax))

;;; Block Comment handling
;;
;; Old version block comments were handled in a special way.
;; Can we simplify with syntax tables?
(defconst matlab-block-comment-start-re "^\\s-*%{\\s-*$"
  "Regexp that matches the beginning of a block comment.
Block comment indicators must be on a line by themselves.")

(defconst matlab-block-comment-end-re "^\\s-*%}\\s-*$"
  "Regexp that matches the end of a block comment.
Block comment indicators must be on a line by themselves.")

(defun matlab-ltype-block-comment-start ()
  "Return non-nil if the current line is a block comment start."
  (save-excursion
    (beginning-of-line)
    (looking-at matlab-block-comment-start-re)))

(defun matlab-ltype-block-comment-end ()
  "Return non-nil if the current line is a block comment start."
  (save-excursion
    (beginning-of-line)
    (looking-at matlab-block-comment-end-re)))

(defun matlab-block-comment-bounds (&optional linebounds)
  "Return start and end positions of block comment if we are in one.
Optional LINEBOUNDS specifies if returned limits are line based instead
of character based."
  (let* ((pps (syntax-ppss (point)))
	 (start (nth 8 pps))
	 (end 0))
    ;; 4 is comment flag.  7 is '2' if block comment
    (when (and (nth 4 pps) (eq (nth 7 pps) 2))
      (save-excursion
	(goto-char start)
	(forward-comment 1)
	(setq end (point)))
      (if linebounds
	  ;; Bounds expanded to beginning/end of the line
	  (cons (save-excursion
		  (goto-char start)
		  (point-at-bol))
		(save-excursion
		  (goto-char end)
		  (point-at-eol)))
	;; Just the bounds
	(cons start end)))))

;;; Navigating Lists
;;
;; MATLAB's lists are (), {}, [].
;; We used to need to do special stuff, but now I think this
;; is just a call striaght to up-list.

(defun matlab-up-list (count &optional restrict)
  "Move forwards or backwards up a list by COUNT.
Optional argument RESTRICT is ignored."
  (save-restriction
    (when restrict (narrow-to-region restrict (point)))
    (let ((dir (if (> count 0) 1 -1)))
      (while (/= count 0)
	(up-list dir nil nil)
	(unless (matlab-cursor-in-string-or-comment)
	  ;; Only count this as moving up a list if we aren't
	  ;; in a comment.
	  (setq count (- count dir)))
	))))

;;; Syntax Compat functions
;;
;; Left over old APIs.  Delete these someday.
(defun matlab-move-simple-sexp-backward-internal (count)
  "Move backward COUNT number of MATLAB sexps."
  (forward-sexp (- count)))

(defun matlab-move-simple-sexp-internal(count)
  "Move over one MATLAB sexp COUNT times.
If COUNT is negative, travel backward."
  (forward-sexp count))

(provide 'matlab-syntax)

;;; matlab-syntax.el ends here
