;;; test-casual-elisp-utils.el --- Casual Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-elisp-test-utils)
(require 'casual-elisp-utils)

(ert-deftest test-casual-elisp-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-elisp-unicode-get :backward-char) "Char"))
    (should (string-equal (casual-elisp-unicode-get :backward-sexp) "Sexp"))
    (should (string-equal (casual-elisp-unicode-get :forward-char) "Char"))
    (should (string-equal (casual-elisp-unicode-get :forward-sexp) "Sexp"))
    (should (string-equal (casual-elisp-unicode-get :previous-line) "Line"))
    (should (string-equal (casual-elisp-unicode-get :next-line) "Line"))
    (should (string-equal (casual-elisp-unicode-get :backward-up-list) "Sexp"))
    (should (string-equal (casual-elisp-unicode-get :down-list) "Sexp")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-elisp-unicode-get :backward-char) "←"))
    (should (string-equal (casual-elisp-unicode-get :backward-sexp) "(←)"))
    (should (string-equal (casual-elisp-unicode-get :forward-char) "→"))
    (should (string-equal (casual-elisp-unicode-get :forward-sexp) "(→)"))
    (should (string-equal (casual-elisp-unicode-get :previous-line) "↑"))
    (should (string-equal (casual-elisp-unicode-get :next-line) "↓"))
    (should (string-equal (casual-elisp-unicode-get :backward-up-list) "(↰"))
    (should (string-equal (casual-elisp-unicode-get :down-list) "⤵("))))

(provide 'test-casual-elisp-utils)
;;; test-casual-elisp-utils.el ends here
