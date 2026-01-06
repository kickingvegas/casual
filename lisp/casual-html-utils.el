;;; casual-html-utils.el --- Casual HTML Utils -*- lexical-binding: t; -*-

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
(require 'sgml-mode)
(require 'casual-lib)

(defconst casual-html-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))
  "Unicode symbol DB to use for HTML Transient menus.")

(defun casual-html-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-html-unicode-db))

(defun casual-html-info ()
  "Open Info for Emacs HTML mode."
  (interactive) (info "(emacs) HTML Mode"))

(provide 'casual-html-utils)
;;; casual-html-utils.el ends here
