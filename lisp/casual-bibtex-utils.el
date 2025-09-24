;;; casual-bibtex-utils.el --- Casual BibTeX Utils -*- lexical-binding: t; -*-

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

(require 'bibtex)
(require 'casual-lib)

(defconst casual-bibtex-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:begin-entry . '("⇱" "Begin"))
    (:end-entry . '("⇲" "End"))
    (:begin-field . '("⇤" "Begin"))
    (:end-field . '("⇥" "End"))
    (:clear . '("⌫" "Clear")))

  "Unicode symbol DB to use for BibTeX Transient menus.")

(defun casual-bibtex-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-bibtex-unicode-db))

(defun casual-bibtex-beginning-of-field (&optional comma)
  "Move point to beginning of BibTeX field value.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list t))
  (let ((bounds (bibtex-enclosing-field comma)))
    (goto-char (bibtex-start-of-text-in-field bounds))
    (if (= (following-char) ?\{)
        (forward-char 1))))

(defun casual-bibtex-end-of-field (&optional comma)
  "Move point to end of BibTeX field value.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."

  (interactive (list t))
  (let ((bounds (bibtex-enclosing-field comma)))
    (goto-char (bibtex-end-of-text-in-field bounds))
    (if (= (preceding-char) ?\})
        (backward-char 1))))

(defun casual-bibtex-copy-field-value (&optional comma)
  "Copy BibTeX field value to `kill-ring'.
Optional arg COMMA is as in `bibtex-enclosing-field'. It is t for
interactive calls."
  (interactive (list t))
  (let* ((bounds (bibtex-enclosing-field comma))
         (value (buffer-substring-no-properties
                 (bibtex-start-of-text-in-field bounds)
                 (bibtex-end-of-text-in-field bounds)))
         (value (string-trim value "{" "}")))
    (message "Copied “%s” to kill-ring" value)
    (kill-new value)))

(provide 'casual-bibtex-utils)
;;; casual-bibtex-utils.el ends here
