;;; test-casual-compile-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-compile-settings)

(ert-deftest test-casual-compile-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-compile--customize-group)
              (casualt-mock #'casual-compile--customize-compilation-scroll-output)
              (casualt-mock #'casual-compile--customize-compilation-auto-jump-to-first-error)
              (casualt-mock #'casual-compile--customize-compilation-max-output-line-length)
              (casualt-mock #'casual-compile-about))

      (let ((test-vectors
             '((:binding "s" :command casual-compile--customize-compilation-scroll-output)
               (:binding "e" :command casual-compile--customize-compilation-auto-jump-to-first-error)
               (:binding "m" :command casual-compile--customize-compilation-max-output-line-length)
               (:binding "G" :command casual-compile--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-compile-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-compile-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-compile-about ()
  (should (stringp (casual-compile-about))))

(provide 'test-casual-compile-settings)
;;; test-casual-compile-setttings.el ends here
