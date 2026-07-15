;;; test-casual-info-utils.el --- Casual Info Utils Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Y. Choi

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
(require 'casual-lib-test-utils)
(require 'casual-info-test-utils)
(require 'casual-info-utils)

(ert-deftest test-casual-info-unicode-get ()
  (let ((casual-info-use-unicode-symbols nil))
    (should (string-equal (casual-info-unicode-get :history-back) "Prev"))
    (should (string-equal (casual-info-unicode-get :history-forward) "Next"))
    (should (string-equal (casual-info-unicode-get :backward-node) "Back"))
    (should (string-equal (casual-info-unicode-get :forward-node) "Forward"))
    (should (string-equal (casual-info-unicode-get :previous) "Prev"))
    (should (string-equal (casual-info-unicode-get :next) "Next"))
    (should (string-equal (casual-info-unicode-get :top-node) "Top"))
    (should (string-equal (casual-info-unicode-get :final-node) "Final"))
    (should (string-equal (casual-info-unicode-get :up-node) "Up"))
    (should (string-equal (casual-info-unicode-get :up-arrow) "Prev"))
    (should (string-equal (casual-info-unicode-get :down-arrow) "Next"))
    (should (string-equal (casual-info-unicode-get :scroll-up) "Up"))
    (should (string-equal (casual-info-unicode-get :scroll-down) "Down"))
    (should (string-equal (casual-info-unicode-get :paragraph) "Paragraph"))
    (should (string-equal (casual-info-unicode-get :link) "Link")))

  (let ((casual-info-use-unicode-symbols t))
    (should (string-equal (casual-info-unicode-get :history-back) "⟨"))
    (should (string-equal (casual-info-unicode-get :history-forward) "⟩"))
    (should (string-equal (casual-info-unicode-get :backward-node) "⟪"))
    (should (string-equal (casual-info-unicode-get :forward-node) "⟫"))
    (should (string-equal (casual-info-unicode-get :previous) "←"))
    (should (string-equal (casual-info-unicode-get :next) "→"))
    (should (string-equal (casual-info-unicode-get :top-node) "⤒"))
    (should (string-equal (casual-info-unicode-get :final-node) "⤓"))
    (should (string-equal (casual-info-unicode-get :up-node) "^"))
    (should (string-equal (casual-info-unicode-get :up-arrow) "↑"))
    (should (string-equal (casual-info-unicode-get :down-arrow) "↓"))
    (should (string-equal (casual-info-unicode-get :scroll-up) "📄↓"))
    (should (string-equal (casual-info-unicode-get :scroll-down) "📄↑"))
    (should (string-equal (casual-info-unicode-get :paragraph) " ¶"))
    (should (string-equal (casual-info-unicode-get :link) " 🔗"))))


(provide 'test-casual-info-utils)
;;; test-casual-info-utils.el ends here
