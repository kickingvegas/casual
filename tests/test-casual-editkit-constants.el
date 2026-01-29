;;; test-casual-editkit-constants.el --- Tests for casual-editkit-constants  -*- lexical-binding: t; -*-

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
    (casualt-editkit-unicode-assert :point-up "Up")
    (casualt-editkit-unicode-assert :point-down "Down")
    (casualt-editkit-unicode-assert :point-left "Left")
    (casualt-editkit-unicode-assert :point-right "Right")
    (casualt-editkit-unicode-assert :window-above "Above")
    (casualt-editkit-unicode-assert :window-below "Below")
    (casualt-editkit-unicode-assert :window-left "To Left")
    (casualt-editkit-unicode-assert :window-right "To Right")
    (casualt-editkit-unicode-assert :other-window "Other")
    (casualt-editkit-unicode-assert :delete-other-windows "Delete other")
    (casualt-editkit-unicode-assert :split-window-below "Below")
    (casualt-editkit-unicode-assert :split-window-horizontally "Right")
    (casualt-editkit-unicode-assert :enlarge "Enlarge")
    (casualt-editkit-unicode-assert :shrink "Shrink")
    (casualt-editkit-unicode-assert :horizontal "Horizontal")
    (casualt-editkit-unicode-assert :vertical "Vertical")
    (casualt-editkit-unicode-assert :first "first")
    (casualt-editkit-unicode-assert :last "last")
    (casualt-editkit-unicode-assert :swap "Swap")
    (casualt-editkit-unicode-assert :jump "Jump"))

  (let ((casual-lib-use-unicode t))
    (casualt-editkit-unicode-assert :previous "↑")
    (casualt-editkit-unicode-assert :next "↓")
    (casualt-editkit-unicode-assert :point-up "↑")
    (casualt-editkit-unicode-assert :point-down "↓")
    (casualt-editkit-unicode-assert :point-left "←")
    (casualt-editkit-unicode-assert :point-right "→")
    (casualt-editkit-unicode-assert :window-above "↑")
    (casualt-editkit-unicode-assert :window-below "↓")
    (casualt-editkit-unicode-assert :window-left "←")
    (casualt-editkit-unicode-assert :window-right "→")
    (casualt-editkit-unicode-assert :other-window "»")
    (casualt-editkit-unicode-assert :delete-other-windows "❏")
    (casualt-editkit-unicode-assert :split-window-below "━")
    (casualt-editkit-unicode-assert :split-window-horizontally "┃")
    (casualt-editkit-unicode-assert :enlarge "+")
    (casualt-editkit-unicode-assert :shrink "−")
    (casualt-editkit-unicode-assert :horizontal "↔︎")
    (casualt-editkit-unicode-assert :vertical "↕︎")
    (casualt-editkit-unicode-assert :first "⤒")
    (casualt-editkit-unicode-assert :last "⤓")
    (casualt-editkit-unicode-assert :swap "⇄")
    (casualt-editkit-unicode-assert :jump "🚀")))

(provide 'test-casual-editkit-constants)
;;; test-casual-editkit-constants.el ends here
