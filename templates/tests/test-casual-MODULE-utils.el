;;; test-casual-$MODULE-utils.el --- Casual Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-$MODULE-test-utils)
(require 'casual-$MODULE-utils)

(ert-deftest test-casual-$MODULE-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-$MODULE-unicode-get :previous) "Previous"))
    (should (string-equal (casual-$MODULE-unicode-get :next) "Next"))
    (should (string-equal (casual-$MODULE-unicode-get :goto) "Goto…"))
    (should (string-equal (casual-$MODULE-unicode-get :follow) "Follow…"))
    (should (string-equal (casual-$MODULE-unicode-get :beginning-of-buffer) "Beginning"))
    (should (string-equal (casual-$MODULE-unicode-get :paragraph) "Paragraph"))
    (should (string-equal (casual-$MODULE-unicode-get :update) "Update"))
    (should (string-equal (casual-$MODULE-unicode-get :kill) "Close"))
    (should (string-equal (casual-$MODULE-unicode-get :see-also) "See Also")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-$MODULE-unicode-get :previous) "↑"))
    (should (string-equal (casual-$MODULE-unicode-get :next) "↓"))
    (should (string-equal (casual-$MODULE-unicode-get :goto) "→"))
    (should (string-equal (casual-$MODULE-unicode-get :follow) "🔗…"))
    (should (string-equal (casual-$MODULE-unicode-get :beginning-of-buffer) "⇱"))
    (should (string-equal (casual-$MODULE-unicode-get :end-of-buffer) "⇲"))
    (should (string-equal (casual-$MODULE-unicode-get :paragraph) "¶"))
    (should (string-equal (casual-$MODULE-unicode-get :update) "⟳"))
    (should (string-equal (casual-$MODULE-unicode-get :kill) "×"))
    (should (string-equal (casual-$MODULE-unicode-get :see-also) "👀"))))


(provide 'test-casual-$MODULE-utils)
;;; test-casual-$MODULE-utils.el ends here
