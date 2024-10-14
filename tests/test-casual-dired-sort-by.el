;;; test-casual-dired-sort-by.el --- Casual Dired Sort By Tests  -*- lexical-binding: t; -*-

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
(require 'casual-dired-test-utils)
(require 'casual-dired-sort-by)

(ert-deftest test-casual-dired-sort-by-tmenu-bindings ()
  (casualt-dired-setup)
  (let ((test-vectors (list)))

    (push (casualt-suffix-test-vector "n" #'casual-dired--sort-by-name) test-vectors)
    (push (casualt-suffix-test-vector "k" #'casual-dired--sort-by-kind) test-vectors)
    (push (casualt-suffix-test-vector "l" #'casual-dired--sort-by-date-last-opened) test-vectors)
    (push (casualt-suffix-test-vector "a" #'casual-dired--sort-by-date-added) test-vectors)
    (push (casualt-suffix-test-vector "m" #'casual-dired--sort-by-date-modified) test-vectors)
    (push (casualt-suffix-test-vector "M" #'casual-dired--sort-by-date-metadata-changed) test-vectors)
    (push (casualt-suffix-test-vector "v" #'casual-dired--sort-by-version) test-vectors)
    (push (casualt-suffix-test-vector "s" #'casual-dired--sort-by-size) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-dired-sort-by-tmenu
                                     '(lambda () (random 5000))))
  (casualt-dired-breakdown t))


(ert-deftest test-casual-dired--mode-name-from-sort ()
  (casualt-dired-setup)

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :name nil)) "Dired: name ↓"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :name t)) "Dired: name ↑"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :kind nil)) "Dired: kind ↓"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :kind t)) "Dired: kind ↑"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-last-opened nil)) "Dired: last ↑"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-last-opened t)) "Dired: last ↓"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-added nil)) "Dired: added ↑"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-added t)) "Dired: added ↓"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-modified nil)) "Dired: modified ↑"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-modified t)) "Dired: modified ↓"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-metadata-changed nil)) "Dired: Δ metadata ↑"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :date-metadata-changed t)) "Dired: Δ metadata ↓"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :version nil)) "Dired: version ↓"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :version t)) "Dired: version ↑"))

  (should (string-equal
           (car (casual-dired--mode-name-from-sort :size nil)) "Dired: size ↑"))
  (should (string-equal
           (car (casual-dired--mode-name-from-sort :size t)) "Dired: size ↓"))

  (casualt-dired-breakdown t))


(provide 'test-casual-dired-sort-by)
;;; test-casual-dired-sort-by.el ends here
