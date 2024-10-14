;;; test-casual-isearch-settings.el --- Casual Re-Builder Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-isearch-test-utils)
(require 'casual-isearch-settings)

(ert-deftest test-casual-isearch-settings-tmenu-bindings ()
  (casualt-setup)
  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "G" #'casual-isearch--customize-group) test-vectors)

    (push (casualt-suffix-test-vector "u" #'casual-lib-customize-casual-lib-use-unicode) test-vectors)
    (push (casualt-suffix-test-vector "n" #'casual-lib-customize-casual-lib-hide-navigation) test-vectors)
    (push (casualt-suffix-test-vector "a" #'casual-isearch-about) test-vectors)
    (push (casualt-suffix-test-vector "v" #'casual-isearch-version) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-isearch-settings-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-isearch-about ()
  (should (stringp (casual-isearch-about))))

(provide 'test-casual-isearch-settings)
;;; test-casual-isearch-setttings.el ends here
