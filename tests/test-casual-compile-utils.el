;;; test-casual-compile-utils.el --- Casual Make Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-compile-test-utils)
(require 'casual-compile-utils)

(ert-deftest test-casual-compile-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-compile-unicode-get :previous) "Previous"))
    (should (string-equal (casual-compile-unicode-get :next) "Next"))
    (should (string-equal (casual-compile-unicode-get :goto) "Goto…"))
    (should (string-equal (casual-compile-unicode-get :refresh) "Refresh"))
    (should (string-equal (casual-compile-unicode-get :kill) "Kill"))
    (should (string-equal (casual-compile-unicode-get :display) "Display")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-compile-unicode-get :previous) "↑"))
    (should (string-equal (casual-compile-unicode-get :next) "↓"))
    (should (string-equal (casual-compile-unicode-get :goto) "🚀"))
    (should (string-equal (casual-compile-unicode-get :refresh) "⟳"))
    (should (string-equal (casual-compile-unicode-get :kill) "×"))
    (should (string-equal (casual-compile-unicode-get :display) "👀"))
    ))


(provide 'test-casual-compile-utils)
;;; test-casual-compile-utils.el ends here
