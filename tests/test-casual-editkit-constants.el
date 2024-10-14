;;; test-casual-editkit-constants.el --- Tests for casual-editkit-constants  -*- lexical-binding: t; -*-

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
(require 'casual-editkit-test-utils)
(require 'casual-editkit-utils)

(defun casualt-unicode-db-assert (key control cmd)
  (let ((test (funcall cmd key)))
    (should (string= test control))))

(defun casualt-editkit-unicode-assert (key control)
  (casualt-unicode-db-assert key control #'casual-editkit-unicode-get))

(ert-deftest test-casual-editkit-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (casualt-editkit-unicode-assert :previous "previous")
    (casualt-editkit-unicode-assert :next "next")
    (casualt-editkit-unicode-assert :first "first")
    (casualt-editkit-unicode-assert :last "last")
    (casualt-editkit-unicode-assert :swap "Swap")
    (casualt-editkit-unicode-assert :jump "Jump"))

  (let ((casual-lib-use-unicode t))
    (casualt-editkit-unicode-assert :previous "↑")
    (casualt-editkit-unicode-assert :next "↓")
    (casualt-editkit-unicode-assert :first "⤒")
    (casualt-editkit-unicode-assert :last "⤓")
    (casualt-editkit-unicode-assert :swap "⇄")
    (casualt-editkit-unicode-assert :jump "🚀")))


(provide 'test-casual-editkit-constants)
;;; test-casual-editkit-constants.el ends here
