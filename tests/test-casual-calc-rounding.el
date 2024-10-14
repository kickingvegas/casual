;;; test-casual-calc-rounding.el --- Casual Rounding Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-test-utils)
(require 'casual-calc-rounding)

(ert-deftest test-casual-calc-rounding-tmenu ()
  (casualt-calc-setup)

  (let* ((test-vectors
          '(("r" . casual-calc--round)
            ("f" . casual-calc--floor)
            ("c" . casual-calc--ceiling)
            ("t" . casual-calc--trunc)))
         (test-vectors (append test-vectors casualt-test-operators-group)))

    ;;(pp test-vectors)
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-rounding-tmenu
                                     '(lambda () (random 5000))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-rounding)
;;; test-casual-calc-rounding.el ends here
