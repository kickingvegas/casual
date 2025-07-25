;;; casual-elisp-utils.el --- Casual Elisp Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'xref)
(require 'elisp-mode)
(require 'casual-lib)

(defconst casual-elisp-unicode-db
  '((:backward-char . '("←" "Char"))
    (:backward-sexp . '("(←)" "Sexp"))
    (:forward-char . '("→" "Char"))
    (:forward-sexp . '("(→)" "Sexp"))

    (:previous-line . '("↑" "Line"))
    (:backward-up-list . '("(↰" "Sexp"))

    (:next-line . '("↓" "Line"))
    (:down-list . '("⤵(" "Sexp")))

  "Unicode symbol DB to use for Elisp Transient menus.")

(defun casual-elisp-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-elisp-unicode-db))

(defun casual-elisp--next-sexp-raw ()
  "Raw implementation to move point to the beginning of the next sexp.

This function has no error checking."
  (forward-sexp 2)
  (backward-sexp))

(defun casual-elisp-next-sexp ()
  "Move point to beginning of the next balanced expression (sexp)."
  (interactive)
  (condition-case nil
      (casual-elisp--next-sexp-raw)
    (error (condition-case nil
               (forward-sexp)
             (error
              (message
               "Unable to move point to next balanced expression (sexp)."))))))

(provide 'casual-elisp-utils)
;;; casual-elisp-utils.el ends here
