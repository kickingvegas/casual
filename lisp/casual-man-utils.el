;;; casual-man-utils.el --- Casual Man Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Y. Choi

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
(require 'man)
(require 'casual-lib)

(defconst casual-man-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:goto . '("→" "Goto…"))
    (:follow . '("🔗…" "Follow…"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:paragraph . '("¶" "Paragraph"))
    (:update . '("⟳" "Update"))
    (:kill . '("×" "Close"))
    (:see-also . '("👀" "See Also")))

  "Unicode symbol DB to use for Man Transient menus.")

(defun casual-man-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-man-unicode-db))

(defun casual-man-occur-options ()
  "Show options for current man page with occur."
  (interactive)
  (occur "^[[:blank:]]*[-+]+[[:alnum:]-=_]*"))

(defun casual-man-info ()
  "Open Info for Emacs Man Page."
  (interactive) (info "(emacs) Man Page"))

(provide 'casual-man-utils)
;;; casual-man-utils.el ends here
