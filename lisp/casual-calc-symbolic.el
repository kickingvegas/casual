;;; casual-calc-symbolic.el --- Casual Symbolic Menu      -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'casual-calc--calc)
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-labels)
(require 'casual-calc-angle-measure)
(require 'casual-calc-graphics)
(require 'casual-calc-algebra)
(require 'casual-calc-variables)
(require 'casual-calc-fileio)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-symbolic-tmenu ()
  "Computer Algebra Menu.
\nCommands to work with algebraic expressions. From here you can
- Manipulate algebraic expressions
- Manipulate polynomial expressions
- Perform Calculus
- Solve expressions either symbolically or numerically
- Curve fit data
- Perform summations"
  ["Computer Algebra"
   ["Manipulation"
    :pad-keys t
    ("E" "Simplify" casual-calc--alg-evaluate :transient t)
    ("=" "Evaluate Variables" casual-calc--evaluate :transient t)
    ("m" "⋯›" casual-calc-symbolic-manipulation-tmenu :transient nil)]
   [""
    :pad-keys t
    ("F" "Formula›" casual-calc-subformula-tmenu :transient nil)]
   casual-calc-operators-group]

  [["Polynomial"
    :pad-keys t
    ("f" "Factor" casual-calc--factor :transient t)
    ("e" "Expand" casual-calc--expand :transient t)
    ("p" "⋯›" casual-calc-polynomial-tmenu :transient nil)]

   ["Calculus"
    :pad-keys t
    ("d" "Derivative…" casual-calc--derivative :transient t)
    ("i" "Integral…" casual-calc--integral :transient t)
    ("c" "⋯›" casual-calc--calculus-tmenu :transient nil)]

   ["Solve"
    ("s" "Symbolic›" casual-calc-solve-symbolic-tmenu :transient nil)
    ("n" "Numeric›" casual-calc-solve-numeric-tmenu :transient nil)]]

  [""
   ["Misc"
    ("C" "Curve Fit›" casual-calc-curve-fit-tmenu :transient nil)
    ("S" "Summations›" casual-calc-summations-tmenu :transient nil)
    ("l" "Equalities & Logic›" casual-calc-symbolic-logic-tmenu :transient nil)
    ("g" "Graphics›" casual-calc-plot-tmenu :transient nil)
    ("z" "Variables›" casual-calc-variable-crud-tmenu :transient nil)]

   ["Settings"
    ("A" calc-algebraic-mode
     :description (lambda ()
                    (casual-lib-checkbox-label calc-algebraic-mode
                                            "Algebraic Mode"))
     :transient t)
    ("M" calc-symbolic-mode :description casual-calc-symbolic-mode-label :transient t)
    ("a" casual-calc-angle-measure-tmenu
     :description (lambda ()
                    (format "Angle Measure (now %s)›"
                            (casual-calc-angle-mode-label)))
     :transient t)]]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-subformula-tmenu ()
  "Sub-formula Menu.
\nCommands to select and edit sub-formulas."
  ["Selection & Navigation"
   [("s" "Select" casual-calc--select-here :transient t)
    ("o" "Select Once" casual-calc--select-once :transient t)
    ("m" "More" casual-calc--select-more :transient t)
    ("p" "Previous" casual-calc--select-previous :transient t)]

   [("u" "Unselect" casual-calc--unselect :transient t)
    ("c" "Clear Selections" casual-calc--clear-selections :transient t)
    ("l" "Less" casual-calc--select-less :transient t)
    ("n" "Next" casual-calc--select-next :transient t)]]

  ["Manipulate"
   [("b" "← Commute" casual-calc--commute-left :transient t)
    ("d" "Distribute" casual-calc--sel-distribute :transient t)
    ("i" "Isolate" casual-calc--sel-isolate :transient t)
    ("N" "Negate" casual-calc--sel-negate :transient t)
    ("e" "⇄" casual-calc--sel-jump-equals :transient t)]

   [("f" "→ Commute" casual-calc--commute-right :transient t)
    ("M" "Merge" casual-calc--sel-merge :transient t)
    ("&" "Invert" casual-calc--sel-invert :transient t)
    ("=" "=" casual-calc--sel-evaluate :transient t)]]

  ["Edit"
   [("`" "Edit" casual-calc--edit-selection :transient nil)
    ("C" "Copy" casual-calc--copy-selection :transient t)]

   [("'" "Replace" casual-calc--enter-selection :transient nil)
    ("D" "Delete" casual-calc--del-selection :transient t)]]

  ;; ["Both Sides"
  ;;  ("*" "Multiply…" casual-calc--sel-mult-both-sides :transient t)
  ;;  ("/" "Divide…" casual-calc--sel-div-both-sides :transient t)
  ;;  ("+" "Add…" casual-calc--sel-add-both-sides :transient t)
  ;;  ("-" "Subtract…" casual-calc--sel-sub-both-sides :transient t)
  ;;  ]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-symbolic-manipulation-tmenu ()
  "Symbolic Manipulation Menu.
Commands to manipulate a symbolic expression."
  [["Symbolic Manipulation"
   ("E" "Simplify" casual-calc--alg-evaluate :transient t)
   ("=" "Evaluate Variables" casual-calc--evaluate :transient t)
   ("e" "Expand Formula" casual-calc--expand-formula :transient t)
   ("m" "Map Equation" casual-calc--map-equation :transient t)
   ("s" "Substitute" casual-calc--substitute :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)


(transient-define-prefix casual-calc-polynomial-tmenu ()
  "Polynomial Menu.
Commands to manipulate a polynomial expression."
  [["Polynomials"
   ("f" "Factor" casual-calc--factor :transient t)
   ("e" "Expand" casual-calc--expand :transient t)
   ("c" "Collect…" casual-calc--collect :transient t)
   ("a" "Apart" casual-calc--apart :transient t)
   ("n" "Normalize Ratio" casual-calc--normalize-rat :transient t)
   ("\\" "Polynomial Divide" casual-calc--poly-div :transient t)
   ("%" "Polynomial Remainder" casual-calc--poly-rem :transient t)
   ("/" "Polynomial Divide & Remainder" casual-calc--poly-div-rem :transient t)
   ("g" "Polynomial GCD" casual-calc--poly-gcd :transient t)]]
  casual-calc-navigation-group)

(transient-define-prefix casual-calc--calculus-tmenu ()
  "Calculus Menu.
Commands to perform Calculus."
  [["Calculus"
   ("n" "Numeric Integral…" casual-calc--num-integral :transient t)
   ("t" "Taylor…" casual-calc--taylor :transient t)
   ("d" "Derivative…" casual-calc--derivative :transient t)
   ("i" "Integral…" casual-calc--integral :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-solve-symbolic-tmenu ()
  "Symbolic Solve Menu.
Commands to solve an algebraic expression symbolically."
  [["Symbolic Solutions"
   ("s" "Solve for…" casual-calc--solve-for :transient t)
   ("p" "Polynomial roots for…" casual-calc--poly-roots :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-solve-numeric-tmenu ()
  "Numerica Solve Menu.
Commands to solve an algebraic expression numerically."
  [["Numerical Solutions"
   ("r" "Find Root" casual-calc--find-root :transient t)
   ("m" "Find Minimum…" casual-calc--find-minimum :transient t)
   ("x" "Find Maximum…" casual-calc--find-maximum :transient t)
   ("h" "Head" casual-calc--head :transient t)
   ("w" "Why" casual-calc--why :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-curve-fit-tmenu ()
  "Curve Fit Menu.
Curve fit commands."
  [["Curve Fit"
   ("c" "Curve Fit" casual-calc--curve-fit :transient t)
   ("p" "Polynomial Interpolation" casual-calc--poly-interp :transient t)
   ("o" "Open Curve Fit Data…" casual-calc-read-curvefit-data :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-summations-tmenu ()
  "Summations Menu.
Summation commands."
  [["Summations"
   ("s" "𝚺" casual-calc--summation :transient t)
   ("a" "𝚺 alternating" casual-calc--alt-summation :transient t)
   ("p" "𝚷" casual-calc--product :transient t)
   ("t" "Tabulate" casual-calc--tabulate :transient t)]
   casual-calc-operators-group]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-symbolic-logic-tmenu ()
  "Symbolic Logic Menu.
Symbolic logic commands."
  ["Equalities"
   [("=" "=" casual-calc--equal-to :transient t)
    ("l" "<" casual-calc--less-than :transient t)
    ("g" ">" casual-calc--greater-than :transient t)]

   [("n" "≠" casual-calc--not-equal-to :transient t)
    ("L" "≤" casual-calc--less-equal :transient t)
    ("G" "≥" casual-calc--greater-equal :transient t)]

   [("x" "Remove Comparator" casual-calc--remove-equal :transient t)]
   casual-calc-operators-group]

  [["Operators"
    ("!" "not (!)" casual-calc--logical-not :transient t)
    ("&" "⋀ (&&)" casual-calc--logical-and :transient t)
    ("|" "⋁ (||)" casual-calc--logical-or :transient t)]

   ["Misc"
    ("e" "∈" casual-calc--in-set :transient t)
    ("i" "if" casual-calc--logical-if :transient t)]]

  casual-calc-navigation-group)


(provide 'casual-calc-symbolic)
;;; casual-calc-symbolic.el ends here
