;;; matlab-wisent.el --- Wisent parser support for MATLAB
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
;; Use the cedet/semantic lexer to analyze MATLAB buffers for tokens to
;; be used for testing and analyzing a buffer to assist with indentation
;; and navigation.

(require 'matlab-syntax)
(require 'matlab-scan)
(require 'semantic/lex)
(require 'cedet/matlab-wy)

;;; Code:

;; Block handling in the lexer
;;
;; MATLAB language doesn't have classic blocks in brackets like other
;; languages that lets us tag up the language without parsing the
;; whole thing.  To overcome that, the lexer wrapper will call the
;; standard lexer over subsetes of the buffer, orchestrated to only
;; cover the parts of the buffer we care about.

(defvar matlab-lex-block-stack nil)
(make-variable-buffer-local 'matlab-lex-block-stack)

(defun matlab-lexer-wrapper (start end &optional depth length)
  "Wrapper around `matlab-lexer'.
Most of the MATLAB syntax we don't want to waste our time parsing.
We already developed a clean way to scan block keywords to handle indenting
so use that to identify keywords, and then use wisent to parse the tokens
related to useful identifiers."
  (unless depth (setq depth semantic-lex-depth))
  (setq matlab-lex-block-stack nil)
  (save-excursion
    (goto-char start)
    (let ((tmp nil)
	  ;; Store stream as a list of lists of tokens.  We'll
	  ;; flatten the list at the end, but in the meantime it
	  ;; makes access quick.
	  (stream-chunks nil)
	  (ignore-until nil)
	  (ogdepth depth)
	  (action-hook
	   (lambda (action &optional keyword)
	     ;; This hook is called whenever a valid
	     (cond
	      ;; PUSH actions mean beginning of a block.
	      ;; Look at the keyword type and decide if we should
	      ;; lex up that line or not.
	      ((eq action 'push)
	       ;;(message "push %S" keyword)
	       (cond
		;; Declarations:
		;;   Stream the entire line.
		((and (eq (car keyword) 'decl) (not ignore-until))
		 (save-excursion
		   (forward-word -1)
		   (push (matlab-lexer (point) end depth length) stream-chunks)))
		;; MCOS items:
		;;    Stream the entire line
		((eq (car keyword) 'mcos) ;; (not ignore-until)) - not needed?
		 (cond
		  ((= depth 0)
		   (let* ((kw (nth 1 keyword))
			  (sym (cond ((string= kw "properties")
				      'PROPERTIES_BLOCK)
				     ((string= kw "methods")
				      'METHODS_BLOCK)
				     ((string= kw "events")
				      'EVENTS_BLOCK)
				     ((string= kw "enumeration")
				      'ENUM_BLOCK)
				     (t nil))))
		     (when sym
		       (push (list (cons sym (cons (nth 2 keyword) (nth 3 keyword))))
			     stream-chunks)
		       (setq ignore-until keyword))))
		  (t
		   (save-excursion
		     (forward-word -1)
		     (setq depth (1- depth))
		     (push (matlab-lexer (point) end depth length) stream-chunks)
		     ))))
		      
		)
	       (push keyword matlab-lex-block-stack)
	       )
		    
	      ;; POP actions mean an end.  Add an end token
	      ;; if it matches something we originally pushed.
	      ((eq action 'pop)
	       ;;(message "pop %S because of %S" (car matlab-lex-block-stack) keyword)
	       (cond
		((and (memq (car (car matlab-lex-block-stack)) '(decl mcos))
		      (not ignore-until))
		 ;; Only stream the END keyword if it is matching
		 ;; one of the things we add start tokens for.
		 (push (list (cons 'END (cons (nth 2 keyword) (nth 3 keyword))))
		       stream-chunks))

		((eq (car (car matlab-lex-block-stack)) 'mcos)
		 (cond
		  ((and ignore-until
			(eq ignore-until (car matlab-lex-block-stack)))
		   ;; We are ending a block - so look back to the last stream
		   ;; and update the end of the BLOCK section.
		   (setcdr (nthcdr 1 (car (car stream-chunks))) (nth 3 keyword))
		   (setq ignore-until nil)
		   )
		  (t
		   (setq depth (1+ depth)))))
		)
	       (pop matlab-lex-block-stack)
	       ))
	     ) )
	  )
      
      (while (setq tmp (matlab--scan-next-keyword 'indent end))
	;; Add this keyword
	(funcall action-hook 'push tmp)

	;; Scan everything inside this block
	(matlab--scan-block-forward end nil action-hook)

	;; Loop around and see if there are more blocks.
	)

      ;; Assemble the lexical stream.
      (let ((stream (apply 'append (nreverse stream-chunks))))
	;;(message "\nDepth %d Stream: %S" ogdepth stream)
	stream))))
  

;;;###autoload
(defun matlab-wisent-default-setup ()
  "Set up a buffer for semantic parsing of the C language."
  (interactive)

  (matlab-wy--install-parser)
    
  (setq semantic-lex-syntax-modifications '((?\. ".")
                                            )
	semantic-type-relation-separator-character '(".")
	semantic-command-separation-character ";"
	;; Names of types for tools like speedbar/imenu
	semantic-symbol->name-assoc-list-for-type-parts
	;; in type parts
	'((type     . "Enumerations")
	  (variable . "Properties")
	  (function . "Methods"))
	;; global names for tags.
	semantic-symbol->name-assoc-list
	(append semantic-symbol->name-assoc-list-for-type-parts
		'((type  . "Classes")
		  (function  . "Functions")))
	
	)

  (setq semantic-lex-analyzer #'matlab-lexer-wrapper)

  ;;(semantic-show-unmatched-syntax-mode 1)
  )

(add-hook 'matlab-mode-hook #'matlab-wisent-default-setup)
;;(remove-hook 'matlab-mode-hook #'matlab-wisent-default-setup)

(provide 'cedet/matlab-wisent)

;;; matlab-lex.el ends here
