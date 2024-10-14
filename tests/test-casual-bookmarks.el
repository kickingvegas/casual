;;; test-casual-bookmarks.el --- Casual IBuffer Tests -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'casual-bookmarks-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-bookmarks)

(ert-deftest test-casual-bookmarks-tmenu-bindings ()
  (casualt-bookmarks-setup)
  (let
      ((test-vectors
        (list
         (casualt-suffix-test-vector "" #'bookmark-bmenu-this-window)
         (casualt-suffix-test-vector "o" #'bookmark-bmenu-other-window)
         (casualt-suffix-test-vector "v" #'bookmark-bmenu-select)
         (casualt-suffix-test-vector "r" #'bookmark-bmenu-rename)
         (casualt-suffix-test-vector "R" #'bookmark-bmenu-relocate)
         (casualt-suffix-test-vector "x" #'bookmark-bmenu-execute-deletions)
         (casualt-suffix-test-vector "+" #'casual-bookmarks-add-bookmark-via-buffer)
         (casualt-suffix-test-vector "w" #'bookmark-bmenu-locate)

         (casualt-suffix-test-vector "m" #'bookmark-bmenu-mark)
         (casualt-suffix-test-vector "M" #'bookmark-bmenu-mark-all)
         (casualt-suffix-test-vector "u" #'bookmark-bmenu-unmark)
         (casualt-suffix-test-vector "U" #'bookmark-bmenu-unmark-all)
         (casualt-suffix-test-vector "d" #'bookmark-bmenu-delete)
         (casualt-suffix-test-vector "D" #'bookmark-bmenu-delete-all)

         (casualt-suffix-test-vector "S" #'casual-bookmarks-sortby-tmenu)
         (casualt-suffix-test-vector "t" #'bookmark-bmenu-toggle-filenames)
         (casualt-suffix-test-vector "/" #'bookmark-bmenu-search)
         (casualt-suffix-test-vector "g" #'revert-buffer)

         (casualt-suffix-test-vector "e" #'bookmark-bmenu-edit-annotation)
         (casualt-suffix-test-vector "a" #'bookmark-bmenu-show-annotation)
         (casualt-suffix-test-vector "A" #'bookmark-bmenu-show-all-annotations)

         (casualt-suffix-test-vector "p" #'previous-line)
         (casualt-suffix-test-vector "n" #'next-line)
         (casualt-suffix-test-vector "<" #'beginning-of-buffer)
         (casualt-suffix-test-vector ">" #'end-of-buffer)
         (casualt-suffix-test-vector "J" #'bookmark-jump)

         (casualt-suffix-test-vector "b" #'tabulated-list-previous-column)
         (casualt-suffix-test-vector "f" #'tabulated-list-next-column)
         (casualt-suffix-test-vector "{" #'tabulated-list-narrow-current-column)
         (casualt-suffix-test-vector "}" #'tabulated-list-widen-current-column)

         (casualt-suffix-test-vector "s" #'bookmark-bmenu-save)
         (casualt-suffix-test-vector "," #'casual-bookmarks-settings-tmenu)
         (casualt-suffix-test-vector "q" #'quit-window))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-bookmarks-tmenu
                                     '(lambda () (random 5000))))
  (casualt-bookmarks-breakdown t))

(provide 'test-casual-bookmarks)
;;; test-casual-bookmarks.el ends here
