;;; casual-info-utils.el --- Casual Info Utils  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

(require 'transient)
(require 'info)
(require 'casual-lib)
(require 'casual-info-variables)

(defconst casual-info-unicode-db
  '((:fast-forward . '("⏩️" "Next"))
    (:rewind . '("⏪️" "Prev"))
    (:fast-forward-or-down . '("⏩️⤵️"  "Next/Down"))
    (:rewind-or-up . '("⏪️⤴️" "Prev/Up"))
    (:up . '("⏫️" "Up"))
    (:first . '("⏮️" "First"))
    (:last . '("⏭️" "Last"))
    (:up-arrow . '("↑" "Prev"))
    (:down-arrow . '("↓" "Next"))
    (:scroll-up . '("📄↓" "Up"))
    (:scroll-down . '("📄↑" "Down"))
    (:paragraph . '(" ¶" "Paragraph"))
    (:link . '(" 🔗" "Link")))
  "Unicode symbol DB to use for Info Transient menus.")

(defun casual-info-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-info-unicode-db))

(defun casual-info-browse-backward-paragraph ()
  "Move point backward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (backward-paragraph 2)
  (forward-line))

(defun casual-info-browse-forward-paragraph ()
  "Move point forward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (forward-paragraph)
  (forward-line))

(provide 'casual-info-utils)
;;; casual-info-utils.el ends here
