;;; test-casual-calc-trail.el --- Casual Trail Menu Tests  -*- lexical-binding: t; -*-

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

(ert-deftest test-casual-calc-trail-tmenu ()
  (casualt-calc-setup)
  (cl-letf
      (((symbol-function #'calc-trail-display) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-in) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-out) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-scroll-left) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-scroll-right) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-previous) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-next) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-first) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-last) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-marker) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-isearch-backward) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-isearch-forward) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-yank) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-trail-kill) (lambda (x) (interactive)(print "WARNING: override")))
       ((symbol-function #'calc-save-modes) (lambda (x) (interactive)(print "WARNING: override"))))
    (let* ((test-vectors '(("d" . calc-trail-display)
                           ("t" . calc-trail-in)
                           ("c" . calc-trail-out)
                           ("<" . calc-trail-scroll-left)
                           (">" . calc-trail-scroll-right)
                           ("p" . calc-trail-previous)
                           ("n" . calc-trail-next)
                           ("[" . calc-trail-first)
                           ("]" . calc-trail-last)
                           ("m" . calc-trail-marker)
                           ("" . calc-trail-isearch-backward)
                           ("" . calc-trail-isearch-forward)
                           ("y" . calc-trail-yank)
                           ("k" . calc-trail-kill)
                           ("s" . calc-save-modes))))
      (casualt-suffix-testbench-runner test-vectors
                                       #'casual-calc-trail-tmenu
                                       '(lambda () (random 5000)))))
  (casualt-calc-breakdown t t))


(provide 'test-casual-calc-trail)
;;; test-casual-calc-trail.el ends here
