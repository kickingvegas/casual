;;; test-casual-calc-vector.el --- Casual Vector/Matrix Menu Tests  -*- lexical-binding: t; -*-

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
(require 'casual-calc-test-utils)
(require 'casual-calc-vector)

(ert-deftest test-casual-calc-vector-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-tmenu
   '(
     ("b|" (2 3) (vec 2 3))
     ;; TODO: define remaining testcases.
     ))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-vector-tmenu-extract ()
  (casualt-calc-setup)
  (calc-push-list '((vec (vec 1 4) (vec 2 3))))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) '(vec 2 3)))

  (calc-push-list '((vec (vec 9 2) (vec 7 14))))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "c2")
  (should (equal (calc-top) '(vec 2 14)))

  (calc-push-list '((vec 7 9)))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "r2")
  (should (equal (calc-top) 9))

  (calc-push-list '((vec 2 3)))
  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "c1")
  (should (= (calc-top) 2))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-vector-building-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-building-tmenu
   '(
     ;; Build
     ("|" (2 3) (vec 2 3))
     ;; TODO: "i" requires handling Emacs prefix convention to override prompt.
     ("e" ((intv 3 0 5)) (vec 0 1 2 3 4 5))
     ;; TODO: "I" requires handling Emacs prefix convention to override prompt.
     ;; TODO: "d" requires handling Emacs prefix convention to override prompt.
     ;; TODO: "b" requires handling Emacs prefix convention to override prompt.

     ;; Transpose
     ("t" ((vec 1 2)) (vec (vec 1) (vec 2)))
     ("r" ((vec 1 2 3)) (vec 3 2 1))
     ;; TODO: "a"
     ("s" ((vec 2 1 3)) (vec 1 2 3))
     ("p" ((vec 2 1 3 3)) (vec 1 2 3))

     ;; Miscellaneous
     ("l" ((vec 2 1 3 3)) 4)
     ("c" ((vec 2 1 3 3)) 4)
     ("f" ((vec 2 1 3 5) 3) 3)
     ;; TODO: "h"
     ))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-vector-arithmetic-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-vector-arithmetic-tmenu
   '(
     ("t" ((vec 2 3)) (vec (vec 2) (vec 3)))
     ("A" ((vec 2 3)) (float 360555127546 -11))
     ("r" ((vec 2 3)) 3)
     ("c" ((vec 2 3)) 5)
     ("p" ((vec 2 4 1) (vec 8 9 2)) (vec -1 4 -14))
     ("k"
      ((vec (vec 2 3) (vec 8 7))
       (vec (vec 9 14) (vec -4 -16)))
      (vec
       (vec 18 28 27 42)
       (vec -8 -32 -12 -48)
       (vec 72 112 63 98)
       (vec -32 -128 -28 -112)))

     ("&"
      ((vec (vec 2 4 1) (vec 8 9 2) (vec 7 12 3)))
      (vec (vec -3 0 1) (vec 10 1 -4) (vec -33 -4 14)))

     ("d" ((vec (vec 2 4 1) (vec 8 9 2) (vec 7 12 3))) -1)
     ("l"
      ((vec (vec 2 4 ) (vec 8 9)))
      (vec
       (vec (vec 0 1) (vec 1 0))
       (vec (vec 1 0) (vec (float 25 -2) 1))
       (vec (vec 8 9) (vec 0 (float 175 -2)))))

     ("T" ((vec (vec 2 4 ) (vec 8 9))) 11)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-statistics-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-statistics-tmenu
   '(("c" ((vec 1 2)) 2)
     ("c" ((vec 1 2 (vec 3))) 3)
     ("c" ((vec (vec 1 2) (vec 3))) 3)
     ("s" ((vec 3 5)) 8)
     ("x" ((vec 3 5)) 5)
     ("m" ((vec 5 5)) 5)
     ("e" ((vec 1 2 3 4 5 6 7 8 9)) (sdev 5 (float 912870929175 -12)))
     ("M" ((vec 1 2 3 4 5 6 7 8 9)) 5)
     ("H" ((vec 1 2 3 4 5 6 7 8 9)) (float 318137186141 -11))
     ("g" ((vec 1 2 3 4 5 6 7 8 9)) (float 41471662744 -10))
     ("r" ((vec 1 2 3 4 5 6 7 8 9)) (float 562731433871 -11))
     ("1" ((vec 1 2 3 4 5 6 7 8 9)) (float 273861278753 -11))
     ("2" ((vec 1 2 3 4 5 6 7 8 9)) (float 258198889747 -11))
     ("3" ((vec 1 2 3 4 5 6 7 8 9)) (float 75 -1))
     ("4" ((vec 1 2 3 4 5 6 7 8 9)) (float 666666666667 -11))
     ("5" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -175 -2))
     ("6" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -14 -1))
     ("7" ((vec 1 2 3 4 5) (vec 5 3 3 4 1)) (float -746202507245 -12))))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-set-operations-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-set-operations-tmenu
   '(("d" ((vec 5 3 3 4 1)) (vec 1 3 4 5))
     ("u" ((vec 1 2) (vec 3 4)) (vec 1 2 3 4))
     ("i" ((vec 1 2 3) (vec 3 4)) (vec 3))
     ("D" ((vec 1 2 3) (vec 3 4)) (vec 1 2))
     ("x" ((vec 1 2 3) (vec 3 4)) (vec 1 2 4))
     ("~" ((vec 3 10)) (vec
                        (intv 2 (neg (var inf var-inf)) 3) (intv 0 3 10)
                        (intv 1 10 (var inf var-inf))))
     ("#" ((vec 5 3 3 4 1)) 4)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-map-and-reduce-tmenu-integration ()
  (casualt-calc-setup)
  (casualt-run-menu-input-testcases
   'casual-calc-map-and-reduce-tmenu
   '(
     ("m+" ((vec 1 2) 1) (vec 2 3))
     ("r+" ((vec 1 2)) 3)
     ("a+" ((vec 2 4)) 6)
     ("A+" ((vec 1 3 8)) (vec 1 4 12))))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc--histogram ()
  (casualt-calc-setup)
  (calc-push '(vec 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
                   19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
                   35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
                   51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66
                   67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82
                   83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98
                   99 100))

  (funcall 'casual-calc-vector-tmenu)
  (execute-kbd-macro "sh[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]")
  (should (equal (calc-top) '(vec 5 10 10 10 10 10 10 10 10 10 5)))
  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-vector-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-vlength) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-transpose) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-reverse-vector) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-sort) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-remove-duplicates) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-mrow) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-mcol) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-pack) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-unpack) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("b" . casual-calc-vector-building-tmenu)
                           ("a" . casual-calc-vector-arithmetic-tmenu)
                           ("s" . casual-calc-statistics-tmenu)
                           ("S" . casual-calc-set-operations-tmenu)
                           ("m" . casual-calc-map-and-reduce-tmenu)
                           ("l" . calc-vlength)
                           ("t" . calc-transpose)
                           ("v" . calc-reverse-vector)
                           ("o" . calc-sort)
                           ("d" . calc-remove-duplicates)
                           ("r" . calc-mrow)
                           ("c" . calc-mcol)
                           ("p" . calc-pack)
                           ("u" . calc-unpack)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-vector-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-vector-building-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-concat) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-index) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-enumerate) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-ident) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-diag) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-build-vector) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-transpose) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-reverse-vector) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-arrange-vector) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-sort) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-remove-duplicates) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vlength) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-count) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-find) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-histogram) (lambda (x) (interactive)(print "WARNING: override")))
       )
    (let* ((test-vectors '(("|" . calc-concat)
                           ("i" . calc-index)
                           ("e" . calc-set-enumerate)
                           ("I" . calc-ident)
                           ("d" . calc-diag)
                           ("b" . calc-build-vector)
                           ("t" . calc-transpose)
                           ("r" . calc-reverse-vector)
                           ("a" . calc-arrange-vector)
                           ("s" . calc-sort)
                           ("p" . calc-remove-duplicates)
                           ("l" . calc-vlength)
                           ("c" . calc-vector-count)
                           ("f" . calc-vector-find)
                           ("h" . calc-histogram)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-vector-building-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(ert-deftest test-casual-calc-vector-arithmetic-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-conj-transpose) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-abs) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-rnorm) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-cnorm) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-cross) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-kron) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-inv) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-mdet) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-mlud) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-mtrace) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'casual-calc-crossp) (lambda () t))
       ((symbol-function #'casual-calc-matrixmultp) (lambda () t))
       ((symbol-function #'casual-calc-square-matrixp) (lambda () t))
       )
    (let* ((test-vectors '(
                           ("t" . calc-conj-transpose)
                           ("A" . calc-abs)
                           ("r" . calc-rnorm)
                           ("c" . calc-cnorm)
                           ("p" . calc-cross)
                           ("k" . calc-kron)
                           ("&" . calc-inv)
                           ("d" . calc-mdet)
                           ("l" . calc-mlud)
                           ("T" . calc-mtrace)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-vector-arithmetic-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-statistics-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-vector-count) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-sum) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-max) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-mean) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-mean-error) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-median) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-harmonic-mean) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-geometric-mean) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-rms) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-sdev) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-pop-sdev) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-variance) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-pop-variance) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-covariance) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-pop-covariance) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-vector-correlation) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("c" . calc-vector-count)
                           ("s" . calc-vector-sum)
                           ("x" . calc-vector-max)
                           ("m" . calc-vector-mean)
                           ("h" . casual-calc--histogram)
                           ("e" . calc-vector-mean-error)
                           ("M" . calc-vector-median)
                           ("H" . calc-vector-harmonic-mean)
                           ("g" . calc-vector-geometric-mean)
                           ("r" . calc-vector-rms)
                           ("1" . calc-vector-sdev)
                           ("2" . calc-vector-pop-sdev)
                           ("3" . calc-vector-variance)
                           ("4" . calc-vector-pop-variance)
                           ("5" . calc-vector-covariance)
                           ("6" . calc-vector-pop-covariance)
                           ("7" . calc-vector-correlation)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-statistics-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-set-operations-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (
       ((symbol-function #'calc-remove-duplicates) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-union) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-intersect) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-difference) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-xor) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-complement) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-set-cardinality) (lambda (x) (interactive)(print "WARNING: override")))
       )
    (let* ((test-vectors '(
                           ("d" . calc-remove-duplicates)
                           ("u" . calc-set-union)
                           ("i" . calc-set-intersect)
                           ("D" . calc-set-difference)
                           ("x" . calc-set-xor)
                           ("~" . calc-set-complement)
                           ("#" . calc-set-cardinality)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-set-operations-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))


(ert-deftest test-casual-calc-map-and-reduce-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-map) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-reduce) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-apply) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-accumulate) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("m" . calc-map)
                           ("r" . calc-reduce)
                           ("a" . calc-apply)
                           ("A" . calc-accumulate)))
           (test-vectors (append test-vectors casualt-test-operators-group)))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-map-and-reduce-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))

(provide 'test-casual-calc-vector)
;;; test-casual-calc-vector.el ends here
