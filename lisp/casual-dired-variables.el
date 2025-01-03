;;; casual-dired-variables.el --- Casual Dired Variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'casual-lib)

(defcustom casual-dired-listing-switches '("--all"
                                           "--human-readable"
                                           "--group-directories-first")
  "List of GNU ‘ls’ arguments used to initialize the “Sort By” menu.

This variable is used to initialize the prefix arguments in the
Transient menu `casual-dired-sort-by-tmenu', provided that there
are no saved prefix values via `transient-save'.  In the event of
the latter, then the saved values are used to populate the menu
and this variable is ignored.  Note that this is a global value
and can not be configured per Dired buffer.

Note that this variable is only observed by the “Sort By”
menu (`casual-dired-sort-by-tmenu').

This variable is designed to not be dependent on
`dired-ls-sorting-switches'. Configure
`dired-ls-sorting-switches' to define the initial sorting of a
Dired buffer upon its creation.

Runtime changes to this variable will not take effect until the
next restart of Emacs.

The switch \"-l\" should not be used as it is automatically
accounted for.

This variable requires GNU ‘ls’ from coreutils installed.

See the man page `ls(1)' for details."
  :type '(repeat string)
  :group 'dired)

(define-obsolete-variable-alias 'casual-dired-use-unicode-symbols
  'casual-lib-use-unicode
  "1.4.0")

(defcustom casual-dired-use-unicode-symbols nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'dired)

(provide 'casual-dired-variables)
;;; casual-dired-variables.el ends here
