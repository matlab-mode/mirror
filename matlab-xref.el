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

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql matlab-shell)))
  (let ((ident (matlab-read-word-at-point)))
    (and (not (equal ident "")) ident)))

(cl-defmethod xref-backend-definitions ((_backend (eql matlab-shell)) prompt)
  (let* ((file-and-flag (matlab-shell-which-fcn prompt))
         (built-in-p (cdr file-and-flag))
         (file (if built-in-p
                   (replace-regexp-in-string "/@[^/]+" "" (car file-and-flag))
                 (car file-and-flag))))
    (when (and (not (equal file "")) (file-exists-p file))
      (list (xref-make prompt
                       (xref-make-file-location
                        file
                        1 0))))))

(cl-defmethod xref-backend-apropos ((_backend (eql matlab-shell)) pattern)
  (xref-backend-definitions 'matlab-shell pattern))

;;;###autoload
(defun matlab-shell-xref-activate ()
  "Function to activate xref backend.
Add this function to `xref-backend-functions' for matlab shell to
use xref to find function and variable definitions."
  (and (member major-mode '(matlab-mode matlab-shell-mode org-mode))
       (matlab-shell-active-p)
       'matlab-shell))

(provide 'matlab-xref)

;;; matlab-xref.el ends here
