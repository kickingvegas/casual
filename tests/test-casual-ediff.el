;;; test-casual-ediff.el --- Casual Make Tests -*- lexical-binding: t; -*-

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
(require 'casual-ediff-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-ediff)

;; Gah, testing casual-ediff-tmenu might be too hard.
;; Resorting to manual testing.

;; (ert-deftest test-casual-ediff-tmenu ()
;;   (let ((tmpfile "casual-ediff-tmenu.txt"))
;;     (casualt-ediff-setup)
;;     (cl-letf (;;((symbol-function #') (lambda () t))
;;               (casualt-mock #'ediff-previous-difference)
;;               (casualt-mock #'ediff-next-difference)
;;               ;;(casualt-mock #')
;;               )

;;       (let ((test-vectors
;;              '((:binding "p" :command ediff-previous-difference)
;;                (:binding "n" :command ediff-next-difference)
;;                ;;(:binding "" :command ediff-main-)
;;                )))

;;         (casualt-suffix-testcase-runner test-vectors
;;                                         #'casual-ediff-tmenu
;;                                         '(lambda () (random 5000)))))
;;     (casualt-ediff-breakdown)))

(provide 'test-casual-ediff)
;;; test-casual-ediff.el ends here
