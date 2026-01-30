;;; test-casual-calc-variables.el --- Test Casual Variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

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
(require 'casual-calc-variables)


(ert-deftest test-casual-calc-variable-crud-tmenu ()
  (casualt-calc-setup)
  (let ((test-vectors '(("s" . casual-calc--store)
                        ("r" . casual-calc--recall)
                        ("c" . casual-calc--unstore)
                        ("e" . casual-calc--edit-variable)
                        ("o" . casual-calc--copy-variable)
                        ("x" . casual-calc--store-exchange)
                        ("p" . casual-calc--permanent-variable)
                        ("O" . casual-calc-open-settings-file)
                        ("i" . casual-calc--insert-variables))))
    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-calc-variable-crud-tmenu
                                     '(lambda () (random 5000))))
  )



(provide 'test-casual-calc-variables)
;;; test-casual-calc-variables.el ends here
