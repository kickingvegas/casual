;;; test-casual-csv-utils.el --- Casual Make Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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
(require 'casual-csv-test-utils)
(require 'casual-csv-utils)

(ert-deftest test-casual-csv-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-csv-unicode-get :up) "Up"))
    (should (string-equal (casual-csv-unicode-get :down) "Down"))
    (should (string-equal (casual-csv-unicode-get :right) "Right"))
    (should (string-equal (casual-csv-unicode-get :left) "Left"))
    (should (string-equal (casual-csv-unicode-get :bol) "Begin"))
    (should (string-equal (casual-csv-unicode-get :eol) "End"))
    (should (string-equal (casual-csv-unicode-get :beginning-of-buffer) "Begin"))
    (should (string-equal (casual-csv-unicode-get :end-of-buffer) "End")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-csv-unicode-get :up) "↑"))
    (should (string-equal (casual-csv-unicode-get :down) "↓"))
    (should (string-equal (casual-csv-unicode-get :right) "→"))
    (should (string-equal (casual-csv-unicode-get :left) "←"))
    (should (string-equal (casual-csv-unicode-get :bol) "⇤"))
    (should (string-equal (casual-csv-unicode-get :eol) "⇥"))
    (should (string-equal (casual-csv-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (casual-csv-unicode-get :end-of-buffer) "⇲"))))


(ert-deftest test-casual-csv-align-tmenu ()
  (let ((tmpfile "casual-csv-align-tmenu.txt"))
    (casualt-csv-setup)
    (cl-letf ((casualt-mock #'csv-align-mode)
              (casualt-mock #'casual-csv-align-auto)
              (casualt-mock #'casual-csv-align-left)
              (casualt-mock #'casual-csv-align-right)
              (casualt-mock #'casual-csv-align-centre))

      (let ((test-vectors
             '((:binding "t" :command csv-align-mode)
               (:binding "a" :command casual-csv-align-auto)
               (:binding "l" :command casual-csv-align-left)
               (:binding "r" :command casual-csv-align-right)
               (:binding "c" :command casual-csv-align-centre))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-csv-align-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-csv-breakdown)))

(provide 'test-casual-csv-utils)
;;; test-casual-csv-utils.el ends here
