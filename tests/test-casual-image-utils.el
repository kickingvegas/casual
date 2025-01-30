;;; test-casual-image-utils.el --- Casual Image Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'casual-image-test-utils)
(require 'casual-image-utils)

(ert-deftest test-casual-image-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-image-unicode-get :rotate) "Rotate Clockwise 90°x…"))
    (should (string-equal (casual-image-unicode-get :scroll-up) "Up"))
    (should (string-equal (casual-image-unicode-get :scroll-down) "Down"))
    (should (string-equal (casual-image-unicode-get :scroll-left) "Left"))
    (should (string-equal (casual-image-unicode-get :scroll-right) "Right"))
    (should (string-equal (casual-image-unicode-get :edge-left) "Left"))
    (should (string-equal (casual-image-unicode-get :edge-right) "Right"))
    (should (string-equal (casual-image-unicode-get :top-left) "Top-left"))
    (should (string-equal (casual-image-unicode-get :bottom-right) "Bottom-right"))
    (should (string-equal (casual-image-unicode-get :previous-image) "Previous Image"))
    (should (string-equal (casual-image-unicode-get :next-image) "Next Image"))
    (should (string-equal (casual-image-unicode-get :dired) "Dired"))
    (should (string-equal (casual-image-unicode-get :mark-image) "Mark Image"))
    (should (string-equal (casual-image-unicode-get :unmark-image) "Unmark Image")))


  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-image-unicode-get :rotate) "Rotate ⟳90°𝑥…"))
    (should (string-equal (casual-image-unicode-get :scroll-up) "↑"))
    (should (string-equal (casual-image-unicode-get :scroll-down) "↓"))
    (should (string-equal (casual-image-unicode-get :scroll-left) "←"))
    (should (string-equal (casual-image-unicode-get :scroll-right) "→"))
    (should (string-equal (casual-image-unicode-get :edge-left) "⇤"))
    (should (string-equal (casual-image-unicode-get :edge-right) "⇥"))
    (should (string-equal (casual-image-unicode-get :top-left) "⇱"))
    (should (string-equal (casual-image-unicode-get :bottom-right) "⇲"))
    (should (string-equal (casual-image-unicode-get :previous-image) "↑🌇"))
    (should (string-equal (casual-image-unicode-get :next-image) "↓🌇"))
    (should (string-equal (casual-image-unicode-get :dired) "🗄️"))
    (should (string-equal (casual-image-unicode-get :mark-image) "Mark 🌇"))
    (should (string-equal (casual-image-unicode-get :unmark-image) "Unmark 🌇"))))


(ert-deftest test-casual-image-resize-tmenu ()
  (let ((tmpfile "casual-image-resize-tmenu.txt"))
    (casualt-image-setup)
    (cl-letf (((symbol-function #'casual-image--identify-label) (lambda () "some image info"))
              (casualt-mock #'casual-image--resize))

      (let ((test-vectors
             '((:binding "r" :command casual-image--resize))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-image-resize-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-image-breakdown)))

(provide 'test-casual-image-utils)
;;; test-casual-image-utils.el ends here
