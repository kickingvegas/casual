;;; test-casual-bookmarks-settings.el --- Casual IBuffer Settings Tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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
(require 'casual-bookmarks-test-utils)
(require 'casual-bookmarks-settings)

(ert-deftest test-casual-bookmarks-settings-tmenu-bindings ()
  (casualt-bookmarks-setup)
  (let
      ((test-vectors
        (list
         (casualt-suffix-test-vector "G" #'casual-bookmarks--customize-group)
         (casualt-suffix-test-vector "u" #'casual-lib-customize-casual-lib-use-unicode)
         (casualt-suffix-test-vector "n" #'casual-lib-customize-casual-lib-hide-navigation)
         (casualt-suffix-test-vector "a" #'casual-bookmarks-about)
 )))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-bookmarks-settings-tmenu
                                     '(lambda () (random 5000))))
  (casualt-bookmarks-breakdown t))

(ert-deftest test-casual-bookmarks-about ()
  (should (stringp (casual-bookmarks-about))))

(provide 'test-casual-bookmarks-settings)
;;; test-casual-bookmarks-setttings.el ends here
