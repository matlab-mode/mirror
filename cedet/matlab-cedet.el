;;; matlab-cedet.el --- Setup CEDET support in MATLAB
;;
;; Copyright (C) 2021 Eric Ludlam
;;
;; Author: Eric Ludlam <eludlam@mathworks.com>
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
;; Setup use of CEDET tools with MATLAB Mode.

(eval-when-compile
  (require 'semantic)
  (require 'srecode/map))

;;; Code:

;;;###autoload
(defun matlab-cedet-setup2 ()
  "Setup CEDET support in MATLAB buffers.
This will install:
  * wiset based parser for matlab tags.
  * srecode template support (todo) "
  (interactive)

  ;; Support parsing buffer for tags.
  (add-hook 'matlab-mode-hook #'matlab-wisent-default-setup)

  ;; Pull in support
  (require 'matlab-semantic)
  
  )

(defun matlab-cedet-uninstall ()
  "Undo CEDET setup for matlab buffers."
  (interactive)

  ;; Remove setup for semantic parsing.
  (remove-hook 'matlab-mode-hook #'matlab-wisent-default-setup)

  )


(provide 'matlab-cedet)

;;; matlab-cedet.el ends here
