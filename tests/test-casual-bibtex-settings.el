;;; test-casual-bibtex-settings.el --- Casual Make Settings Tests  -*- lexical-binding: t; -*-

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
(require 'casual-bibtex-test-utils)
(require 'casual-bibtex-settings)

(ert-deftest test-casual-bibtex-settings-tmenu ()
  (let ()
    (cl-letf ((casualt-mock #'casual-bibtex--customize-dialect)
              (casualt-mock #'casual-bibtex--customize-file-path)
              (casualt-mock #'casual-bibtex--customize-files)
              (casualt-mock #'casual-bibtex--customize-search-entry-globally)
              (casualt-mock #'casual-bibtex--customize-clean-entry-hook)
              (casualt-mock #'casual-bibtex--customize-add-entry-hook)
              (casualt-mock #'casual-bibtex--customize-group)
              (casualt-mock #'casual-bibtex-about))

      (let ((test-vectors
             '((:binding "d" :command casual-bibtex--customize-dialect)
               (:binding "G" :command casual-bibtex--customize-group)
               (:binding "p" :command casual-bibtex--customize-file-path)
               (:binding "f" :command casual-bibtex--customize-files)
               (:binding "g" :command casual-bibtex--customize-search-entry-globally)
               (:binding "C" :command casual-bibtex--customize-clean-entry-hook)
               (:binding "A" :command casual-bibtex--customize-add-entry-hook)
               (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
               (:binding "n" :command casual-lib-customize-casual-lib-hide-navigation)
               (:binding "a" :command casual-bibtex-about))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-bibtex-settings-tmenu
                                        '(lambda () (random 5000)))))))

(ert-deftest test-casual-bibtex-about ()
  (should (stringp (casual-bibtex-about))))

(provide 'test-casual-bibtex-settings)
;;; test-casual-bibtex-settings.el ends here
