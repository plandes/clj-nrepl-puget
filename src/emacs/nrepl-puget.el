;;; nrepl-puget.el --- Cider overloads to enable color output in nREPL

;; Copyright © 2017 Paul Landes
;;
;; Author: Paul Landes <landes@mailc.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Cider overloads to enable color output in nREPL

;;; Code:

(require 'cider-util)
(require 'ansi-color)

(defvar cider-font-lock-as-old (symbol-function 'cider-font-lock-as))

(defun nrepl-puget-detect-ansi-p (string)
  "Return non-nil if STRING is an ANSI string."
  ;"[35m1[0m"
  (string-match "^\\[" string))

(defun cider-font-lock-as (mode string)
  "Use MODE to font-lock the STRING."
  (if (nrepl-puget-detect-ansi-p string)
      (ansi-color-apply string)
    (funcall cider-font-lock-as-old mode string)))

(provide 'nrepl-puget)

;;; nrepl-puget.el ends here
