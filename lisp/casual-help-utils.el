;;; casual-help-utils.el --- Casual Help Utils -*- lexical-binding: t; -*-

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
(require 'help-mode)
(require 'casual-lib)

(defgroup casual-help nil
  "Settings for Casual Help."
  :group 'casual)

(defcustom casual-help-add-extra-keybindings t
  "If non-nil then extra module-specific keybindings will be set.

See `casual-help-setup' for more detail for said keybindings."
  :type 'boolean
  :group 'casual-help)

(defconst casual-help-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:forward . '("→" "Forward"))
    (:backward . '("←" "Backward"))
    (:goto . '("→" "Goto…"))
    (:link . '("🔗" "Link"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:paragraph . '("¶" "Paragraph"))
    (:page . '("📄" "Page")))

  "Unicode symbol DB to use for Help Transient menus.")

(defun casual-help-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-help-unicode-db))


(defun casual-help-info ()
  "Open Info for Emacs Help Mode."
  (interactive) (info "(emacs) Help Mode"))

(defun casual-help--current-data-p ()
  "Predicate if help mode current data exists."
  help-mode--current-data)

(defun casual-help--symbolp ()
  "Predicate if current help mode has a symbol."
  (let ((sym (plist-get help-mode--current-data :symbol)))
    (or (boundp sym) (facep sym))))

(provide 'casual-help-utils)
;;; casual-help-utils.el ends here
