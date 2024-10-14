;;; test-casual-lib.el --- Casual Avy Tests          -*- lexical-binding: t; -*-

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
(require 'casual-lib-test-utils)
(require 'casual-lib)

;; Test Unicode DB
(defconst casual-lib-unicode-db
  '((:scope . '("⬍" "#")))
  "Unicode symbol DB to use for Casual Transient menus.")

(ert-deftest test-casual-lib-use-unicode ()
  (should (symbolp 'casual-lib-use-unicode)))

(ert-deftest test-casual-lib-unicode-db ()
  (let* ((item (eval (alist-get :scope casual-lib-unicode-db))))
    (should (string-equal "#" (nth 1 item)))
    (should (string-equal "⬍" (nth 0 item)))))

(ert-deftest test-casual-lib-unicode-get ()
  (let ((casual-lib-use-unicode t))
    (should (string-equal "⬍" (casual-lib-unicode-db-get :scope casual-lib-unicode-db))))

  (let ((casual-lib-use-unicode nil))
    (should (string-equal "#" (casual-lib-unicode-db-get :scope casual-lib-unicode-db)))))

(defun casual-foo-unicode-get (key)
  "Example porcelain getter for KEY."
  (casual-lib-unicode-db-get key casual-lib-unicode-db))

(ert-deftest test-casual-foo-unicode-get ()
  (let ((casual-lib-use-unicode t))
    (should (string-equal "⬍" (casual-foo-unicode-get :scope))))

  (let ((casual-lib-use-unicode nil))
    (should (string-equal "#" (casual-foo-unicode-get :scope)))))

;; Test Labels
(ert-deftest test-casual-lib--variable-to-checkbox ()
  (let ((casual-lib-use-unicode nil))
    (should (string-equal "[x]" (casual-lib--variable-to-checkbox t)))
    (should (string-equal "[ ]" (casual-lib--variable-to-checkbox nil))))

  (let ((casual-lib-use-unicode t))
    (should (string-equal "☑︎" (casual-lib--variable-to-checkbox t)))
    (should (string-equal "◻︎" (casual-lib--variable-to-checkbox nil)))))

(ert-deftest test-casual-lib--prefix-label ()
  (let ((label "foo")
        (prefix "bar"))
    (should (string-equal "bar foo" (casual-lib--prefix-label label prefix)))))

(ert-deftest test-casual-lib--suffix-label ()
  (let ((label "foo")
        (suffix "bar"))
    (should (string-equal "foo bar" (casual-lib--suffix-label label suffix)))))

(ert-deftest test-casual-lib-checkbox-label ()
  (let ((casual-lib-use-unicode nil)
        (label "bingo"))
    (should (string-equal "[x] bingo" (casual-lib-checkbox-label t label)))
    (should (string-equal "[ ] bingo" (casual-lib-checkbox-label nil label))))

  (let ((casual-lib-use-unicode t)
        (label "bingo"))
    (should (string-equal "☑︎ bingo" (casual-lib-checkbox-label t label)))
    (should (string-equal "◻︎ bingo" (casual-lib-checkbox-label nil label)))))

;; Test Predicates
(ert-deftest test-casual-lib-display-line-numbers-mode-p ()
  (let ((display-line-numbers nil))
    (should (not (casual-lib-display-line-numbers-mode-p))))

  (let ((display-line-numbers 'relative))
    (should (casual-lib-display-line-numbers-mode-p))))

(ert-deftest test-casual-lib-buffer-writeable-p ()
  (with-temp-buffer
    (should (casual-lib-buffer-writeable-p)))

  (with-temp-buffer
    (read-only-mode)
    (should (not (casual-lib-buffer-writeable-p)))))

;; Dunno why this works interactively but not in batch
;; (ert-deftest test-casual-lib-buffer-writeable-and-region-active-p ()
;;   (with-temp-buffer
;;     (set-mark 0)
;;     (insert "hey there")
;;     (end-of-buffer)
;;     (should (casual-lib-buffer-writeable-and-region-active-p))))

(ert-deftest test-casual-lib-hide-navigation ()
  (should (symbolp 'casual-lib-hide-navigation)))

(ert-deftest test-casual-lib-hide-navigation-p ()
  (let ((casual-lib-hide-navigation t))
    (should (equal t (casual-lib-hide-navigation-p))))

  (let ((casual-lib-hide-navigation nil))
    (should (equal nil (casual-lib-hide-navigation-p)))))


(ert-deftest test-casual-lib-quit-all-hide-navigation-p ()
  (let ((casual-lib-hide-navigation nil)
        (transient--stack nil))
    (should (equal (casual-lib-quit-all-hide-navigation-p) t)))

  (let ((casual-lib-hide-navigation nil)
        (transient--stack t))
    (should (equal (casual-lib-quit-all-hide-navigation-p) nil)))

  (let ((casual-lib-hide-navigation t)
        (transient--stack nil))
    (should (equal (casual-lib-quit-all-hide-navigation-p) t)))

  (let ((casual-lib-hide-navigation t)
        (transient--stack t))
    (should (equal (casual-lib-quit-all-hide-navigation-p) t))))

(provide 'test-casual-lib)
;;; test-casual-lib.el ends here
