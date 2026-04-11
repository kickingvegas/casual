;;; casual-editkit-constants.el --- Constants file for Casual EditKit  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools, wp

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
(require 'simple)
(require 'casual-lib)

(defconst casual-editkit-unicode-db
  '((:previous . '("↑" "previous"))
    (:next . '("↓" "next"))
    (:point-up . '("↑" "Up"))
    (:point-down . '("↓" "Down"))
    (:point-left . '("←" "Left"))
    (:point-right . '("→" "Right"))

    (:window-above . '("↑" "Above"))
    (:window-below . '("↓" "Below"))
    (:window-left . '("←" "To Left"))
    (:window-right . '("→" "To Right"))

    (:other-window . '("»" "Other"))
    (:delete-other-windows . '("❏" "Delete other"))
    (:split-window-below . '("━" "Below"))
    (:split-window-horizontally . '("┃" "Right"))
    (:enlarge . '("+" "Enlarge"))
    (:shrink . '("−" "Shrink"))
    (:horizontal . '("↔︎" "Horizontal"))
    (:vertical . '("↕︎" "Vertical"))
    (:first . '("⤒" "first"))
    (:last . '("⤓" "last"))
    (:swap . '("⇄" "Swap"))
    (:jump . '("🚀" "Jump")))
  "Unicode symbol DB to use for Bookmarks Transient menus.")

(defun casual-editkit-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-editkit-unicode-db))

;; Transient navigation group for Casual EditKit menus.
(transient-define-group casual-editkit-navigation-group
  [:class transient-row
   (casual-lib-quit-one)
   ("U" "Undo" undo :transient t)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

;; Transient cursor navigation group for Casual EditKit menus.
(transient-define-group casual-editkit-cursor-navigation-group
  ["Cursor"
   :class transient-row
   ("<left>" "←" backward-char :transient t)
   ("<right>" "→" forward-char :transient t)
   ("<up>" "↑" previous-line :transient t)
   ("<down>" "↓" next-line :transient t)])

(provide 'casual-editkit-constants)
;;; casual-editkit-constants.el ends here
