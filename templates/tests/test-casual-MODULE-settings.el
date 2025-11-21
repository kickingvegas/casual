;;; test-casual-$MODULE-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-$MODULE-settings)

(ert-deftest test-casual-$MODULE-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-$MODULE--customize-group)
              (casualt-mock #'casual-$MODULE-about))

      (let ((test-vectors
             '((:binding "G" :command casual-$MODULE--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-$MODULE-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-$MODULE-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-$MODULE-about ()
  (should (stringp (casual-$MODULE-about))))

(provide 'test-casual-$MODULE-settings)
;;; test-casual-$MODULE-setttings.el ends here
