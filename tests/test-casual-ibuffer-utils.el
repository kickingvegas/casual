;;; test-casual-ibuffer-utils.el --- Casual IBuffer Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-ibuffer-test-utils)
(require 'casual-ibuffer-utils)

(ert-deftest test-casual-ibuffer-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-ibuffer-unicode-get :previous) "Previous"))
    (should (string-equal (casual-ibuffer-unicode-get :next) "Next"))
    (should (string-equal (casual-ibuffer-unicode-get :jump) "Jump"))
    (should (string-equal (casual-ibuffer-unicode-get :marked) "Marked"))
    (should (string-equal (casual-ibuffer-unicode-get :group) ""))
    (should (string-equal (casual-ibuffer-unicode-get :jumpgroup) "Jump to Group")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-ibuffer-unicode-get :previous) "↑"))
    (should (string-equal (casual-ibuffer-unicode-get :next) "↓"))
    (should (string-equal (casual-ibuffer-unicode-get :jump) "🚀"))
    (should (string-equal (casual-ibuffer-unicode-get :marked) "❯"))
    (should (string-equal (casual-ibuffer-unicode-get :group) "[]"))
    (should (string-equal (casual-ibuffer-unicode-get :jumpgroup) "🚀[]"))))

(provide 'test-casual-ibuffer-utils)
;;; test-casual-ibuffer-utils.el ends here
