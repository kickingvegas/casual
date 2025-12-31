;;; test-casual-css.el --- Casual Make Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'casual-css-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-css)

(ert-deftest test-casual-css-tmenu ()
  (let ((temp-file-name "casual-css-test.css"))
    (casualt-css-setup)

    (cl-letf ((casualt-mock #'css-lookup-symbol)
              (casualt-mock #'css-cycle-color-format)
              (casualt-mock #'fill-paragraph)
              (casualt-mock #'hl-line-mode)
              (casualt-mock #'casual-css-settings-tmenu)
              )



      (let ((test-vectors
             '((:binding "l" :command css-lookup-symbol)
               (:binding "c" :command css-cycle-color-format)
               (:binding "f" :command fill-paragraph)
               (:binding "h" :command hl-line-mode)
               (:binding "," :command casual-css-settings-tmenu)

               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-css-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-css-breakdown)))

(provide 'test-casual-css)
;;; test-casual-css.el ends here
