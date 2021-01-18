;;; matlab-xref.el --- Support xref via matlab-shell
;;
;; Copyright (C) 2021 Karthik Chikmagalur
;;
;; Author: Karthik Chikmagalur
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
;; Support xref API for jumping to tags via use of MATLAB shell.

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-prompt "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

;;; Code:

;;; MATLAB-SHELL -------------
(cl-defmethod xref-backend-identifier-at-point ((_backend (eql matlab-xref-shell)))
  (let ((ident (matlab-read-word-at-point)))
    (and (not (equal ident "")) ident)))

(cl-defmethod xref-backend-definitions ((_backend (eql matlab-xref-shell)) prompt)
  (let* ((file-and-flag (matlab-shell-which-fcn prompt))
         (built-in-p (cdr file-and-flag))
         (file (if built-in-p
                   (replace-regexp-in-string "/@[^/]+" "" (car file-and-flag))
                 (car file-and-flag))))
    (if (and (not (equal file "")) (file-exists-p file))
	(list (xref-make prompt
			 (xref-make-file-location
                          file
                          1 0)))
      ;; If no match - try the local version instead.
      (xref-backend-definitions 'matlab-xref-local prompt)
      )))

(cl-defmethod xref-backend-apropos ((_backend (eql matlab-xref-shell)) pattern)
  (xref-backend-definitions 'matlab-xref-shell pattern))

;;;###autoload
(defun matlab-shell-xref-activate ()
  "Function to activate xref backend that uses MATLAB Shell.
Add this function to `xref-backend-functions' for matlab shell to
use xref to find function and variable definitions."
  (and (matlab-shell-active-p) 'matlab-xref-shell))

;;; MATLAB Local ---------------

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql matlab-xref-local)))
  (let ((ident (matlab-read-word-at-point)))
    (and (not (equal ident "")) ident)))

(cl-defmethod xref-backend-definitions ((_backend (eql matlab-xref-local)) prompt)
  (let* ((name-and-loc (car (matlab-xref-local-function prompt)))
         )
    (when name-and-loc
      (list (xref-make prompt
                       (xref-make-file-location
                        (buffer-file-name)
			(nth 1 name-and-loc)
			(nth 2 name-and-loc)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql matlab-xref-local)) pattern)
  (xref-backend-definitions 'matlab-xref-local pattern))

;;;###autoload
(defun matlab-local-xref-activate ()
  "Function to activate xref backend that uses local buffer scan.
Add this function to `xref-backend-functions' to use xref to find
function and variable definitions."
  'matlab-xref-local)


;;; Helper functions
(defun matlab-xref-local-function (name)
  "Return a list of user defined functions that that match NAME."
  (matlab-navigation-syntax
    (let ((syms (save-excursion
		  (goto-char (point-min))
		  (let ((lst nil))
		    (while (re-search-forward "^\\s-*function\\>" nil t)
		      (if (re-search-forward
			   (concat "\\(" name "\\)\\s-*\\($\\|(\\)")
			   (matlab-point-at-eol) t)
			  (save-excursion
			    (goto-char (match-beginning 0))
			    (setq lst (cons (list (match-string 1)
						  (line-number-at-pos (point) t)
						  (current-column))
					    lst)))))
		    (nreverse lst)))))
      syms)))


(provide 'matlab-xref)

;;; matlab-xref.el ends here
