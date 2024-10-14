;;; test-casual-bookmarks-utils.el --- Casual IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-bookmarks-utils)

(defun casualt-unicode-db-assert (key control cmd)
  (let ((test (funcall cmd key)))
    (should (string= test control))))

(defun casualt-bookmarks-unicode-assert (key control)
  (casualt-unicode-db-assert key control #'casual-bookmarks-unicode-get))

(ert-deftest test-casual-bookmarks-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (casualt-bookmarks-unicode-assert :previous "Previous")
    (casualt-bookmarks-unicode-assert :next "Next")
    (casualt-bookmarks-unicode-assert :beginning-of-buffer "Beginning")
    (casualt-bookmarks-unicode-assert :end-of-buffer "End")
    (casualt-bookmarks-unicode-assert :backward "Backward")
    (casualt-bookmarks-unicode-assert :forward "Forward")
    (casualt-bookmarks-unicode-assert :narrow "Narrow")
    (casualt-bookmarks-unicode-assert :widen "Widen")
    (casualt-bookmarks-unicode-assert :jump "Jump"))

  (let ((casual-lib-use-unicode t))
    (casualt-bookmarks-unicode-assert :previous "↑")
    (casualt-bookmarks-unicode-assert :next "↓")
    (casualt-bookmarks-unicode-assert :beginning-of-buffer "⤒")
    (casualt-bookmarks-unicode-assert :end-of-buffer "⤓")
    (casualt-bookmarks-unicode-assert :backward "←")
    (casualt-bookmarks-unicode-assert :forward "→")
    (casualt-bookmarks-unicode-assert :narrow "→←")
    (casualt-bookmarks-unicode-assert :widen "←→")
    (casualt-bookmarks-unicode-assert :jump "🚀")))

(provide 'test-casual-bookmarks-utils)
;;; test-casual-bookmarks-utils.el ends here
