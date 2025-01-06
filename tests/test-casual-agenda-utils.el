;;; test-casual-agenda-utils.el --- Casual Agenda Utils Tests  -*- lexical-binding: t; -*-

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
(require 'casual-agenda-test-utils)
(require 'casual-agenda-utils)

(ert-deftest test-casual-agenda-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal (casual-agenda-unicode-get :previous) "Prev"))
    (should (string-equal (casual-agenda-unicode-get :next) "Next"))
    (should (string-equal (casual-agenda-unicode-get :up) "Up"))
    (should (string-equal (casual-agenda-unicode-get :down) "Down"))
    (should (string-equal (casual-agenda-unicode-get :jumpdate) "Date"))
    (should (string-equal (casual-agenda-unicode-get :jumpclocked) "Clocked"))
    (should (string-equal (casual-agenda-unicode-get :jump) "Jump"))
    (should (string-equal (casual-agenda-unicode-get :date) "Date"))
    (should (string-equal (casual-agenda-unicode-get :heading) "*"))
    (should (string-equal (casual-agenda-unicode-get :timer) "Timer"))
    (should (string-equal (casual-agenda-unicode-get :sunrise) "Sunrise"))
    (should (string-equal (casual-agenda-unicode-get :lunar) "Lunar"))
    (should (string-equal (casual-agenda-unicode-get :jumpbookmark) "Jump to bookmark"))
    (should (string-equal (casual-agenda-unicode-get :clock) "Clock")))

  (let ((casual-lib-use-unicode t))
    (should (string-equal (casual-agenda-unicode-get :previous) "↑"))
    (should (string-equal (casual-agenda-unicode-get :next) "↓"))
    (should (string-equal (casual-agenda-unicode-get :up) "↑"))
    (should (string-equal (casual-agenda-unicode-get :down) "↓"))
    (should (string-equal (casual-agenda-unicode-get :jumpdate) "🚀 📅"))
    (should (string-equal (casual-agenda-unicode-get :jumpclocked) "🚀 ⏰"))
    (should (string-equal (casual-agenda-unicode-get :jump) "🚀"))
    (should (string-equal (casual-agenda-unicode-get :date) "📅"))
    (should (string-equal (casual-agenda-unicode-get :heading) "✲"))
    (should (string-equal (casual-agenda-unicode-get :timer) "⏱️"))
    (should (string-equal (casual-agenda-unicode-get :sunrise) "🌅"))
    (should (string-equal (casual-agenda-unicode-get :lunar) "🌙"))
    (should (string-equal (casual-agenda-unicode-get :jumpbookmark) "🚀 🔖"))
    (should (string-equal (casual-agenda-unicode-get :clock) "⏰"))))

(provide 'test-casual-agenda-utils)
;;; test-casual-agenda-utils.el ends here
