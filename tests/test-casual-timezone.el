;;; test-casual-timezone.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-timezone-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-timezone)

(ert-deftest test-casual-timezone-tmenu ()
  (let ()
    (casualt-timezone-setup)
    (cl-letf ((casualt-mock #'casual-timezone-local-time-to-remote)
              (casualt-mock #'casual-timezone-remote-time-to-local)
              (casualt-mock #'casual-timezone-planner))

      (let ((test-vectors
             '((:binding "l" :command casual-timezone-local-time-to-remote)
               (:binding "r" :command casual-timezone-remote-time-to-local)
               (:binding "z" :command casual-timezone-planner))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-timezone-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-timezone-breakdown)))

(provide 'test-casual-timezone)
;;; test-casual-timezone.el ends here
