;;; casual-bookmarks-utils.el --- Casual Bookmarks Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Charles Y. Choi

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
(require 'casual-lib)

(defconst casual-bookmarks-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:beginning-of-buffer . '("⤒" "Beginning"))
    (:end-of-buffer . '("⤓" "End"))
    (:backward . '("←" "Backward"))
    (:forward . '("→" "Forward"))
    (:narrow . '("→←" "Narrow"))
    (:widen . '("←→" "Widen"))
    (:jump . '("🚀" "Jump")))
  "Unicode symbol DB to use for Bookmarks Transient menus.")

(defun casual-bookmarks-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-bookmarks-unicode-db))

(provide 'casual-bookmarks-utils)
;;; casual-bookmarks-utils.el ends here
