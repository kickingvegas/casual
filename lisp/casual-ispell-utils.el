;;; casual-ispell-utils.el --- Casual Ispell Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'ispell)
(require 'bibtex)
(require 'conf-mode)
(require 'casual-lib)

(defcustom casual-ispell-comment-or-string-predicate
  #'casual-ispell-comment-or-string-p
  "Function for Ispell comment or string predicate.

By default, the value of this variable is
`casual-ispell-comment-or-string-p'. If this predicate is insufficient,
you can override it with different implementation."
  :type 'function
  :group 'casual)

(defun casual-ispell-info ()
  "Open Info for the Emacs spell checker `ispell'."
  (interactive) (info "(emacs) Spelling"))

(defun casual-ispell-comment-or-string-p ()
  "Predicate for spell check in comment or string.

The predicate is non-nil if the major mode is derived from any of the
following modes:

- `prog-mode'
- `bibtex-mode'
- `conf-mode'"
  (or (derived-mode-p 'prog-mode)
      (derived-mode-p 'bibtex-mode)
      (derived-mode-p 'conf-mode)))

(provide 'casual-ispell-utils)
;;; casual-ispell-utils.el ends here
