;;; test-casual-ispell-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Y. Choi

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
(require 'casual-ispell-test-utils)
(require 'casual-ispell-settings)

(ert-deftest test-casual-ispell-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-ispell--customize-group)
              (casualt-mock #'casual-ispell-about)
              (casualt-mock
               #'casual-ispell--customize-ispell-comment-or-string-predicate))

      (let ((test-vectors
             '((:binding "c" :command casual-ispell--customize-ispell-comment-or-string-predicate)
               (:binding "G" :command casual-ispell--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-ispell-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-ispell-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-ispell-about ()
  (should (stringp (casual-ispell-about))))

(provide 'test-casual-ispell-settings)
;;; test-casual-ispell-setttings.el ends here
