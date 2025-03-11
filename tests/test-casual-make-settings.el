;;; test-casual-make-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-make-test-utils)
(require 'casual-make-settings)

(ert-deftest test-casual-make-settings-tmenu ()
  (let ((tmpfile "casual-make-settings-tmenu.txt"))
    (casualt-make-setup)
    (cl-letf (;;((symbol-function #') (lambda () t))
              (casualt-mock #'casual-make--customize-group))

      (let ((test-vectors
             '((:binding "G" :command casual-make--customize-group)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-make-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-make-settings-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-make-breakdown)))

(ert-deftest test-casual-make-about ()
  (should (stringp (casual-make-about))))

(provide 'test-casual-make-settings)
;;; test-casual-make-setttings.el ends here
