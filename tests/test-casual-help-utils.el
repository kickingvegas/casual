;;; test-casual-help-utils.el --- Casual Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-help-test-utils)
(require 'casual-help-utils)

(ert-deftest test-casual-help-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-help-unicode-get :previous) "Previous"))
    (should (string-equal (casual-help-unicode-get :next) "Next"))
    (should (string-equal (casual-help-unicode-get :forward) "Forward"))
    (should (string-equal (casual-help-unicode-get :backward) "Backward"))
    (should (string-equal (casual-help-unicode-get :goto) "Goto…"))
    (should (string-equal (casual-help-unicode-get :link) "Link"))
    (should (string-equal (casual-help-unicode-get :beginning-of-buffer) "Beginning"))
    (should (string-equal (casual-help-unicode-get :end-of-buffer) "End"))
    (should (string-equal (casual-help-unicode-get :paragraph) "Paragraph"))
    (should (string-equal (casual-help-unicode-get :page) "Page")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-help-unicode-get :previous) "↑"))
    (should (string-equal (casual-help-unicode-get :next) "↓"))
    (should (string-equal (casual-help-unicode-get :goto) "→"))
    (should (string-equal (casual-help-unicode-get :link) "🔗"))
    (should (string-equal (casual-help-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (casual-help-unicode-get :end-of-buffer) "⇲"))
    (should (string-equal (casual-help-unicode-get :paragraph) "¶"))
    (should (string-equal (casual-help-unicode-get :page) "📄"))))

(provide 'test-casual-help-utils)
;;; test-casual-help-utils.el ends here
