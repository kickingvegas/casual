;;; test-casual-csv-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-csv-test-utils)
(require 'casual-csv-settings)

(ert-deftest test-casual-csv-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-csv--customize-align-style)
              (casualt-mock #'casual-csv--customize-separators)
              (casualt-mock #'casual-csv--customize-invisibility-default)
              (casualt-mock #'casual-csv--customize-group)
              (casualt-mock #'casual-csv--customize-header-lines)
              (casualt-mock #'casual-csv--customize-comment-start-default)
              (casualt-mock #'casual-csv--customize-field-quotes)
              (casualt-mock #'casual-csv--customize-align-min-width)
              (casualt-mock #'casual-csv--customize-align-max-width)
              (casualt-mock #'casual-csv-about))

      (let ((test-vectors
             '(
               (:binding "A" :command casual-csv--customize-align-style)
               (:binding "s" :command casual-csv--customize-separators)
               (:binding "i" :command casual-csv--customize-invisibility-default)
               (:binding "G" :command casual-csv--customize-group)
               (:binding "h" :command casual-csv--customize-header-lines)
               (:binding "c" :command casual-csv--customize-comment-start-default)
               (:binding "f" :command casual-csv--customize-field-quotes)
               (:binding "w" :command casual-csv--customize-align-min-width)
               (:binding "W" :command casual-csv--customize-align-max-width)

               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-csv-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-csv-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-csv-about ()
  (should (stringp (casual-csv-about))))

(provide 'test-casual-csv-settings)
;;; test-casual-csv-setttings.el ends here
