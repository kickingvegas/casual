;;; test-casual-$MODULE.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-lib-test-utils)
(require 'casual-$MODULE)

(ert-deftest test-casual-$MODULE-tmenu ()
  (let ()
    (casualt-$MODULE-setup)

    (cl-letf ((casualt-mock #'quit-window))

      (let ((test-vectors
             '((:binding "q" :command quit-window)
               )))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-$MODULE-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-$MODULE-breakdown)))

(provide 'test-casual-$MODULE)
;;; test-casual-$MODULE.el ends here
