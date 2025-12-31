;;; test-casual-html-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-html-test-utils)
(require 'casual-html-settings)

(ert-deftest test-casual-html-settings-tmenu ()
  (let ()
    (cl-letf (
              (casualt-mock #'casual-html--customize-sgml-basic-offset)
              (casualt-mock #'casual-html--customize-sgml-attribute-offset)
              (casualt-mock #'casual-html--customize-html-ts-mode-indent-offset)
              (casualt-mock #'casual-html--customize-group-sgml)
              (casualt-mock #'casual-html-about))

      (let ((test-vectors
             '((:binding "b" :command casual-html--customize-sgml-basic-offset)
               (:binding "A" :command casual-html--customize-sgml-attribute-offset)
               (:binding "G" :command casual-html--customize-group-sgml)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-html-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-html-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-html-about ()
  (should (stringp (casual-html-about))))

(provide 'test-casual-html-settings)
;;; test-casual-html-setttings.el ends here
