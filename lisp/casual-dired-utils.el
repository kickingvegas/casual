;;; casual-dired-utils.el --- Casual Dired Utils Menu  -*- lexical-binding: t; -*-

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
(require 'dired)
(require 'dired-x)
(require 'checkdoc)
(require 'elint)
(require 'casual-lib)
(require 'casual-dired-variables)

(defconst casual-dired-unicode-db
  '((:up-arrow . '("↑" "Up"))
    (:down-arrow . '("↓" "Down"))
    (:goto . '("→" "Goto"))
    (:directory . '("📁" "Dir"))
    (:file . '("📄" "File"))
    (:subdir . '("🗂️" "Subdir")))
  "Unicode symbol DB to use for Dired Transient menus.")

(defun casual-dired-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode' is
non-nil, then the Unicode symbol is returned, otherwise a plain
ASCII-range string."
  (casual-lib-unicode-db-get key casual-dired-unicode-db))

(transient-define-prefix casual-dired-utils-tmenu ()
  ["Utils - Marked Files or File under Point"
   ["Files"
    ("F" "Open" dired-do-find-marked-files :transient nil)
    ("Z" "Compress" dired-do-compress :transient nil)]

   ["Rename"
    ("u" "Upcase…" dired-upcase :transient nil)
    ("d" "Downcase…" dired-downcase :transient nil)]

   ["Categories"
    ("s" "Search & Replace›" casual-dired-search-replace-tmenu :transient nil)
    ("e" "Emacs Lisp›" casual-dired-elisp-tmenu :transient nil)
    ("l" "Link›" casual-dired-link-tmenu :transient nil)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(transient-define-prefix casual-dired-search-replace-tmenu ()
  ["Search & Replace"
   ["Search in Files"
     :pad-keys t
     ("C-s" "I-search…" dired-do-isearch :transient nil)
     ("M-s" "I-search regexp…" dired-do-isearch-regexp :transient nil)
     ("s" "Search first regexp match…" dired-do-search :transient nil)]

    ["Replace in Files"
     ("r" "Query regexp and replace…" dired-do-query-replace-regexp :transient nil)]]

  ["grep-style regex"
   ("g" "Find regex…" dired-do-find-regexp :transient nil)
   ("G" "Find regex and replace…" dired-do-find-regexp-and-replace :transient nil)]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(transient-define-prefix casual-dired-elisp-tmenu ()
  ["Emacs Lisp"
   ["Byte-Compile"
    ("B" "Byte-compile…" dired-do-byte-compile :transient nil)
    ("D" "Byte-compile directory…" byte-recompile-directory :transient nil)]

   ["Load"
    ("L" "Load" dired-do-load :transient nil)]

   ["Verification"
    ("e" "Lint Directory…" elint-directory :transient nil)
    ("c" "Check documentation strings" checkdoc-dired :transient nil)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(transient-define-prefix casual-dired-link-tmenu ()
  ["Link"
   ["Symbolic"
    ("s" "Absolute…" dired-do-symlink)
    ("S" "Absolute regexp…" dired-do-symlink-regexp)]

   ["Relative"
    ("r" "Relative…" dired-do-relsymlink)
    ("R" "Relative regexp…" dired-do-relsymlink-regexp)]

   ["Hard"
    ("h" "Hard…" dired-do-hardlink)
    ("H" "Hard regexp…" dired-do-hardlink-regexp)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(provide 'casual-dired-utils)
;;; casual-dired-utils.el ends here
