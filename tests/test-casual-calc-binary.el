;;; test-casual-calc-binary.el --- Casual Binary Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-binary)

(ert-deftest test-casual-calc-binary-tmenu-bindings ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-group-char) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-leading-zeros) (lambda (x) (interactive)(print "WARNING: override"))))
    (let ((test-vectors '(("&" . casual-calc--and)
                        ("|" . casual-calc--or)
                        ("^" . casual-calc--xor)
                        ("d" . casual-calc--diff)
                        ("!" . casual-calc--not)

                        ("l" . casual-calc--lshift-binary)
                        ("r" . casual-calc--rshift-binary)

                        ("ì" . casual-calc--lshift-arith)
                        ("ò" . casual-calc--rshift-arith)
                        ("" . casual-calc--rotate-binary)

                        ("z" . calc-leading-zeros)
                        ("," . calc-group-char)
                        ("R" . casual-calc-radix-tmenu)
                        ("w" . casual-calc--word-size)
                        ("u" . casual-calc--unpack-bits)
                        ("p" . casual-calc--pack-bits))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-binary-tmenu
                                       '(lambda () (random 5000)))))

  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc--and ()
  (casualt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--and)
  (should (= (calc-top) 8))

  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--or ()
  (casualt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--or)
  (should (= (calc-top) 15))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--xor ()
  (casualt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--xor)
  (should (= (calc-top) 7))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--diff ()
  (casualt-calc-setup)
  (calc-push 11)
  (calc-push 12)
  (call-interactively #'casual-calc--diff)
  (should (= (calc-top) 3))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--not ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 11)
  (call-interactively #'casual-calc--not)
  (should (= (calc-top) 244))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--lshift-binary ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'casual-calc--lshift-binary)
  (should (= (calc-top) 24))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--rshift-binary ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--rshift-binary)
  (should (= (calc-top) 12))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--lshift-arith ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 12)
  (call-interactively #'casual-calc--lshift-arith)
  (should (= (calc-top) 24))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--rshift-arith ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--rshift-arith)
  (should (= (calc-top) 12))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--rotate-binary ()
  (casualt-calc-setup)
  (calc-word-size 8)
  (calc-push 24)
  (call-interactively #'casual-calc--rotate-binary)
  (should (= (calc-top) 48))
  (casualt-calc-breakdown t))

;; (ert-deftest test-casual-calc--word-size ()
;;   (casualt-calc-setup)
;;   (casualt-calc-breakdown t))

;; (ert-deftest test-casual-calc--unpack-bits ()
;;   (casualt-calc-setup)
;;   (casualt-calc-breakdown t))

;; (ert-deftest test-casual-calc--pack-bits ()
;;   (casualt-calc-setup)
;;   (casualt-calc-breakdown t))

(provide 'test-casual-calc-binary)
;;; test-casual-calc-binary.el ends here
