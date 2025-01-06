;;; casual-calc-algebra.el --- Casual wrapped algebra functions  -*- lexical-binding: t; -*-

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
(require 'calc)
(require 'calc-ext)
(require 'calcalg3)

;;(require 'casual-calc-labels)

(defun casual-calc-interval-prompt ()
  "Prompt user for an interval."
  (interactive)
  (let* ((interval (math-read-expr (read-string "Interval: "))))
    (calc-push interval)))

(defun casual-calc--find-minimum ()
  "Find minimum value of an algebraic equation.
\nThis command issues two interactive prompts:
1) an interval that specifies the range of interest to find a minimum value.
2) the variable of interest to find the minimum for.

This command returns a vector containing the value of the
variable which minimizes the formula's value, along with the
minimum value itself.

This function is a wrapper over `calc-find-minimum'.

* References
- info node `(calc) Minimization'
- info node `(calc) Interval Forms'
- `calc-find-minimum'"
  (interactive)
  (calc-push (math-read-expr (read-string "Interval to find minimum: ")))
  (call-interactively #'calc-find-minimum))

(defun casual-calc--find-maximum ()
  "Find maximum value of an algebraic equation.
\nThis command issues two interactive prompts:
1) an interval that specifies the range of interest to find a maximum value.
2) the variable of interest to find the maximum for.

This command returns a vector containing the value of the
variable which maximizes the formula's value, along with the
maximum value itself.

This function is a wrapper over `calc-find-maximum'.

* References
- info node `(calc) Minimization'
- info node `(calc) Interval Forms'
- `calc-find-maximum'"
  (interactive)
  (calc-push (math-read-expr (read-string "Interval to find maximum: ")))
  (call-interactively #'calc-find-maximum))

(defun casual-calc--curve-fit ()
  "Fit a set of data (‘x’ and ‘y’ vectors of numbers) to an equation.
\nThis command attempts to fit a set of data (‘x’ and ‘y’ vectors
of numbers) to a straight line, polynomial, or other function of ‘x’.
\nStack Arguments:
1: [[<x data>] [<y data>]]

Both the vectors of <x data> and <y data> are of equal length.

This command issues two prompts in succession:
1. The model to fit the data to.
2. Variable(s) used to represent the model.

By default this command chooses high letters like ‘x’ and ‘y’ for
independent variables and low letters like ‘a’ and ‘b’ for
parameters.  (The dependent variable doesn't need a name.)  The two
kinds of variables are separated by a semicolon.  Since you
generally care more about the names of the independent variables
than of the parameters, Calc also allows you to name only those
and let the parameters use default names.

* Model Types
- ‘1’   Linear or multilinear.  a + b x + c y + d z.
- ‘2-9’ Polynomials.  a + b x + c x^2 + d x^3.
- ‘e’   Exponential.  a exp(b x) exp(c y).
- ‘E’   Base-10 exponential.  a 10^(b x) 10^(c y).
- ‘x’   Exponential (alternate notation).  exp(a + b x + c y).
- ‘X’   Base-10 exponential (alternate).  10^(a + b x + c y).
- ‘l’   Logarithmic.  a + b ln(x) + c ln(y).
- ‘L’   Base-10 logarithmic.  a + b log10(x) + c log10(y).
- ‘^’   General exponential.  a b^x c^y.
- ‘p’   Power law.  a x^b y^c.
- ‘q’   Quadratic.  a + b (x-c)^2 + d (x-e)^2.
- ‘g’   Gaussian.  (a / b sqrt(2 pi)) exp(-0.5*((x-c)/b)^2).
- ‘s’   Logistic _s_ curve.  a/(1 + exp(b (x - c))).
- ‘b’   Logistic bell curve.  a exp(b (x - c))/(1 + exp(b (x - c)))^2.
- ‘o’   Hubbert linearization.  (y/x) = a (1 - x/b).

This function is a wrapper over `calc-curve-fit'.

* References
- info node `(calc) Curve Fitting'
- info node `(calc) Standard Nonlinear Models'
- `calc-curve-fit'"
  (interactive)
  (call-interactively #'calc-curve-fit))


(defun casual-calc--alg-evaluate ()
  "Perform the normal default simplications on a formula.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-alg-evaluate'.

* References
- info node `(calc) Algebraic Manipulation'
- `calc-alg-evaluate'"
  (interactive)
  (call-interactively #'calc-alg-evaluate))

(defun casual-calc--alt-summation ()
  "Computes alternating sum of a formula over a certain range of index values.
\nSuccessive terms of the sequence are given alternating
signs, with the first term (corresponding to the lower index value)
being positive.  Alternating sums are converted to normal sums with an
extra term of the form ‘(-1)^(k-LOW)’.  This formula is adjusted
appropriately if the step value is other than one.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-alt-summation'.

* References
- info node `(calc) Summations'
- `calc-alt-summation'"
  (interactive)
  (call-interactively #'calc-alt-summation))

(defun casual-calc--apart ()
  "Expand a rational function by partial fractions.
\nA rational function is the quotient of
two polynomials; ‘apart’ pulls this apart into a sum of rational
functions with simple denominators.
\nStack Arguments:
1: equation

This function is a wrapper over `calc-apart'.

* References
- info node `(calc) Polynomials'
- `calc-apart'"
  (interactive)
  (call-interactively #'calc-apart))

(defun casual-calc--clear-selections ()
  "Unselect all selections.
\nThis function is a wrapper over `calc-clear-selections'.

* References
- info node `(calc) Making Selections'
- `calc-clear-selections'"
  (interactive)
  (call-interactively #'calc-clear-selections))

(defun casual-calc--collect ()
  "Rearrange a formula in decreasing powers of a variable.
\nThis command rearranges a formula as a polynomial in a given
variable, ordered in decreasing powers of that variable.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-collect'.

* References
- info node `(calc) Polynomials'
- `calc-collect'"
  (interactive)
  (call-interactively #'calc-collect))

(defun casual-calc--commute-left ()
  "Move the selected sub-formula to the left in its surrounding formula.
\nGenerally the selection is one term of a sum or product; the
sum or product is rearranged according to the commutative laws of
algebra.

This function is a wrapper over `calc-commute-left'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-commute-left'"
  (interactive)
  (call-interactively #'calc-commute-left))

(defun casual-calc--commute-right ()
  "Move the selected sub-formula to the right in its surrounding formula.
\nGenerally the selection is one term of a sum or product; the
sum or product is rearranged according to the commutative laws of
algebra.

This function is a wrapper over `calc-commute-right'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-commute-right'"
  (interactive)
  (call-interactively #'calc-commute-right))

(defun casual-calc--copy-selection ()
  "Copy the selected portion of the formula.
\nThis command copies the selected portion of the formula indicated
by the cursor, or, in the absence of a selection, the entire
formula.

This function is a wrapper over `calc-copy-selection'.

* References
- info node `(calc) Operating on Selections'
- `calc-copy-selection'"
  (interactive)
  (call-interactively #'calc-copy-selection))

(defun casual-calc--del-selection ()
  "Delete the selected portion of the formula.
\nThis command deletes the selected portion of the formula
indicated by the cursor, or, in the absence of a selection, it
deletes the sub-formula indicated by the cursor position.

This function is a wrapper over `calc-del-selection'.

* References
- info node `(calc) Operating on Selections'
- `calc-del-selection'"
  (interactive)
  (call-interactively #'calc-del-selection))

(defun casual-calc--derivative ()
  "Compute the deriviative of a formula with respect to a specified variable.
\nThis command computes the derivative of the expression on the
top of the stack with respect to some variable, which it will
prompt you to enter.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-derivative'.

* References
- info node `(calc) Differentiation'
- `calc-derivative'"
  (interactive)
  (call-interactively #'calc-derivative))

(defun casual-calc--edit-selection ()
  "Edit the selected sub-formula in a separate buffer.

This function is a wrapper over `calc-edit-selection'.

* References
- info node `(calc) Operating on Selections'
- `calc-edit-selection'"
  (interactive)
  (call-interactively #'calc-edit-selection))

(defun casual-calc--enter-selection ()
  "Replace selected sub-formula via algebraic entry.
\nThis command does an algebraic entry just like the regular ‘'’
key.  When you press <RET>, the formula you type replaces the
original selection.  You can use the ‘$’ symbol in the formula to
refer to the original selection.  If there is no selection in the
formula under the cursor, the cursor is used to make a temporary
selection for the purposes of the command.

This function is a wrapper over `calc-enter-selection'.

* References
- info node `(calc) Operating on Selections'
- `calc-enter-selection'"
  (interactive)
  (call-interactively #'calc-enter-selection))

(defun casual-calc--equal-to ()
  "Compare top two elements in the stack if they are equal.
\nTest if the top two elements in the stack are equal, either
because they are identical expressions, or because they are
numbers which are numerically equal.  (Thus the integer 1 is
considered equal to the float 1.0.)  If the equality of ‘a’ and
‘b’ cannot be determined, the comparison is left in symbolic
form.  Note that as a command, this operation pops two values from
the stack and pushes back either a 1 or a 0, or a formula ‘a = b’
if the values' equality cannot be determined.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-equal-to'.

* References
- info node `(calc) Logical Operations'
- `calc-equal-to'"
  (interactive)
  (call-interactively #'calc-equal-to))

(defun casual-calc--expand ()
  "Expand an expression by applying the distributive law everywhere.
\nThis command applies to products, quotients, and powers
involving sums.  By default, it fully distributes all parts of
the expression.
\nStack Argument:
1: expression

This function is a wrapper over `calc-expand'.

* References
- info node `(calc) Polynomials'
- `calc-expand'"
  (interactive)
  (call-interactively #'calc-expand))

(defun casual-calc--expand-formula ()
  "Expands functions into their defining formulas wherever possible.
\nStack Argument:
1: formula

This function is a wrapper over `calc-expand-formula'.

* References
- info node `(calc) Algebraic Manipulation'
- `calc-expand-formula'"
  (interactive)
  (call-interactively #'calc-expand-formula))

(defun casual-calc--factor ()
  "Factor a polynomial expression into a product of terms.
\nStack Argument:
1: expression

This function is a wrapper over `calc-factor'.

* References
- info node `(calc) Polynomials'
- `calc-factor'"
  (interactive)
  (call-interactively #'calc-factor))

(defun casual-calc--find-root ()
  "Find a numerical solution (or “root”) of an equation.
\nThis command treats inequalities the same as equations.  If the
input is any other kind of formula, it is interpreted as an
equation of the form ‘X = 0’.
\nStack Argument:
1: equation

This function is a wrapper over `calc-find-root'.

* References
- info node `(calc) Root Finding'
- `calc-find-root'"
  (interactive)
  (calc-push (math-read-expr (read-string "Interval to find root(s): ")))
  (call-interactively #'calc-find-root))

(defun casual-calc--greater-equal ()
  "Compare top two elements in the stack if they are ≥ to each other.
\nThis command tests a ≥ b and places the result in the top of stack.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-greater-equal'.

* References
- info node `(calc) Logical Operations'
- `calc-greater-equal'"
  (interactive)
  (call-interactively #'calc-greater-equal))

(defun casual-calc--greater-than ()
  "Compare top two elements in the stack if they are > to each other.
\nThis command tests a > b and places the result in the top of stack.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-greater-than'.

* References
- info node `(calc) Logical Operations'
- `calc-greater-than'"
  (interactive)
  (call-interactively #'calc-greater-than))

(defun casual-calc--head ()
  "Extract the first element of the result vector, discarding the error term.

This function is a wrapper over `calc-head'.

* References
- info node `(calc) Root Finding'
- `calc-head'"
  (interactive)
  (call-interactively #'calc-head))

(defun casual-calc--why ()
  "Provide explanation for why no root was found.
\nIn the event that `casual-calc--find-root' is unable to find a
solution and leaves the function in symbolic form on the stack,
this command will provide an explanation as to why.
\nStack Arguments:
1: expression

* References
- info node `(calc) Root Finding'"
  (interactive)
  (call-interactively #'calc-why))

(defun casual-calc--in-set ()
  "Test if a ∈ b.
\nThis command returns true if ‘a’ is in the set ‘b’.  ‘b’ can be
either a vector or an interval.  If ‘b’ is an interval form, ‘a’
must be one of the values encompassed by the interval.  If ‘b’ is
a vector, ‘a’ must be equal to one of the elements of the vector.
If any vector elements are intervals, ‘a’ must be in any of the
intervals.  If ‘b’ is a plain number, ‘a’ must be numerically
equal to ‘b’.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-in-set'.

* References
- info node `(calc) Logical Operations'
- `calc-in-set'"
  (interactive)
  (call-interactively #'calc-in-set))

(defun casual-calc--integral ()
  "Compute the indefinite integral of an expression.
\nThis command computes the indefinite integral of the expression
on the top of the stack with respect to a prompted-for variable.
The integrator is not guaranteed to work for all integrable
functions, but it is able to integrate several large classes of
formulas.  In particular, any polynomial or rational function (a
polynomial divided by a polynomial) is acceptable.  (Rational
functions don't have to be in explicit quotient form, however;
‘x/(1+x^-2)’ is not strictly a quotient of polynomials, but it is
equivalent to ‘x^3/(x^2+1)’, which is.)  Also, square roots of
terms involving ‘x’ and ‘x^2’ may appear in rational functions
being integrated.  Finally, rational functions involving
trigonometric or hyperbolic functions can be integrated.
\nStack Arguments:
1: expression

This function is a wrapper over `calc-integral'.

* References
- info node `(calc) Integration'
- `calc-integral'"
  (interactive)
  (call-interactively #'calc-integral))

(defun casual-calc--less-equal ()
  "Compare top two elements in the stack if they are ≤ to each other.
\nThis command tests a ≤ b and places the result in the top of stack.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-less-equal'.

* References
- info node `(calc) Logical Operations'
- `calc-less-equal'"
  (interactive)
  (call-interactively #'calc-less-equal))

(defun casual-calc--less-than ()
  "Compare top two elements in the stack if they are < to each other.
\nThis command tests a < b and places the result in the top of stack.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-less-than'.

* References
- info node `(calc) Logical Operations'
- `calc-less-than'"
  (interactive)
  (call-interactively #'calc-less-than))

(defun casual-calc--logical-and ()
  "Logical a ∧ b.
\nEvaluate true if both a and b are true, i.e., are non-zero numbers.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-logical-and'.

* References
- info node `(calc) Logical Operations'
- `calc-logical-and'"
  (interactive)
  (call-interactively #'calc-logical-and))

(defun casual-calc--logical-if ()
  "If ‘a’ non-zero then ‘b’, else ‘c’.
\nIf ‘a’ is not a number, the test is left in symbolic form and
neither ‘b’ nor ‘c’ is evaluated in any way.  In algebraic
formulas, this is one of the few Calc functions whose arguments
are not automatically evaluated when the function itself is
evaluated.
\nStack Arguments:
3: a
2: b
1: c

This function is a wrapper over `calc-logical-if'.

* References
- info node `(calc) Logical Operations'
- `calc-logical-if'"
  (interactive)
  (call-interactively #'calc-logical-if))

(defun casual-calc--logical-not ()
  "Evaluate true (1) if top of stack is zero, else false (0).
\nStack Argument:
1: a

This function is a wrapper over `calc-logical-not'.

* References
- info node `(calc) Logical Operations'
- `calc-logical-not'"
  (interactive)
  (call-interactively #'calc-logical-not))

(defun casual-calc--logical-or ()
  "Logical a ∨ b.
\nEvaluate true if either a and b are true, i.e., are non-zero numbers.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-logical-or'.

* References
- info node `(calc) Logical Operations'
- `calc-logical-or'"
  (interactive)
  (call-interactively #'calc-logical-or))

(defun casual-calc--map-equation ()
  "Apply a given function or operator to an equation.
\nStack Arguments:
1: equation

This function is a wrapper over `calc-map-equation'.

* References
- info node `(calc) Algebraic Manipulation'
- `calc-map-equation'"
  (interactive)
  (call-interactively #'calc-map-equation))

(defun casual-calc--normalize-rat ()
  "Rearrange a formula into a quotient of two polynomials, if possible.
\nFor example, given ‘1 + (a + b/c) / d’, the result would be
‘(b + a c + c d) / c d’.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-normalize-rat'.

* References
- info node `(calc) Polynomials'
- `calc-normalize-rat'"
  (interactive)
  (call-interactively #'calc-normalize-rat))

(defun casual-calc--not-equal-to ()
  "Compare top two elements in the stack if they are not equal.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-not-equal-to'.

* References
- info node `(calc) Logical Operations'
- `calc-not-equal-to'"
  (interactive)
  (call-interactively #'calc-not-equal-to))

(defun casual-calc--num-integral ()
  "Calculate numerical integration of expression.
\nThis command prompts for an integration variable, a lower
limit, and an upper limit.  Except for the integration variable,
all other variables that appear in the integrand formula must
have stored values.  (A stored value, if any, for the integration
variable itself is ignored.)
\nStack Argument:
1: expression

This function is a wrapper over `calc-num-integral'.

* References
- info node `(calc) Numerical Integration'
- `calc-num-integral'"
  (interactive)
  (call-interactively #'calc-num-integral))

(defun casual-calc--poly-div ()
  "Divide two polynomials and return the quotient ‘q’, discarding remainder ‘r’.
\nFor any formulas ‘a’ and ‘b’, this command returns ‘q’ such that:
  ‘a = q b + r’
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-poly-div'.

* References
- info node `(calc) Polynomials'
- `calc-poly-div'"
  (interactive)
  (call-interactively #'calc-poly-div))

(defun casual-calc--poly-div-rem ()
  "Divide two polynomials and return vector [‘q’, ‘r’].
\nFor any formulas ‘a’ and ‘b’, this command returns the quotient
‘q’ and remainder ‘r’ as vector [‘q’, ‘r’] such that:
  ‘a = q b + r’
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-poly-div-rem'.

* References
- info node `(calc) Polynomials'
- `calc-poly-div-rem'"
  (interactive)
  (call-interactively #'calc-poly-div-rem))

(defun casual-calc--poly-gcd ()
  "Computes the greatest common divisor of two polynomials.
\nThe GCD actually is unique only to within a constant
multiplier; Calc attempts to choose a GCD which will be
unsurprising.
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-poly-gcd'.

* References
- info node `(calc) Polynomials'
- `calc-poly-gcd'"
  (interactive)
  (call-interactively #'calc-poly-gcd))

(defun casual-calc--poly-interp ()
  "Polynomial interpolate 2-D data (x, y) at a particular value of x.
\nThis command attempts to fit a set of data (‘x’ and ‘y’ vectors
of numbers) at a particular ‘x’ value.
\nStack Arguments:
2: [[<x data>] [<y data>]]
1: x

\nStack Arguments:
1: n

This function is a wrapper over `calc-poly-interp'.

* References
- info node `(calc) Interpolation'
- `calc-poly-interp'"
  (interactive)
  (call-interactively #'calc-poly-interp))

(defun casual-calc--poly-rem ()
  "Divide two polynomials and keep the remainder ‘r’, discarding quotient ‘q’.
\nFor any formulas ‘a’ and ‘b’, this command returns ‘r’ such that:
  ‘a = q b + r’
\nStack Arguments:
2: a
1: b

This function is a wrapper over `calc-poly-rem'.

* References
- info node `(calc) Polynomials'
- `calc-poly-rem'"
  (interactive)
  (call-interactively #'calc-poly-rem))

(defun casual-calc--poly-roots ()
  "Solve for all roots, if possible.
\nThis command tries to solve an equation in general form, then for
all arbitrary-sign variables, then for all arbitrary-integer
variables.  The results are collected into a vector and then
returned.
\nStack Arguments:
1: equation

This function is a wrapper over `calc-poly-roots'.

* References
- info node `(calc) Multiple Solutions'
- `calc-poly-roots'"
  (interactive)
  (call-interactively #'calc-poly-roots))

(defun casual-calc--product ()
  "Computes the product of a formula over a certain range of index values.
\nThe formula is taken from the top of the stack; the command
prompts for the name of the product index variable, the lower
limit of the product (any formula), and the upper limit of the product.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-product'.

* References
- info node `(calc) Summations'
- `calc-product'"
  (interactive)
  (call-interactively #'calc-product))

(defun casual-calc--remove-equal ()
  "Extract the righthand side of the equation or inequality.
\nStack Arguments:
1: expression

This function is a wrapper over `calc-remove-equal'.

* References
- info node `(calc) Logical Operations'
- `calc-remove-equal'"
  (interactive)
  (call-interactively #'calc-remove-equal))

(defun casual-calc--sel-distribute ()
  "Mix the selected sum or product using the distributive law.
\nThis command mixes the selected sum or product into the
surrounding formula using the distributive law.

This function is a wrapper over `calc-sel-distribute'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-distribute'"
  (interactive)
  (call-interactively #'calc-sel-distribute))

(defun casual-calc--sel-commute ()
  "Swap the arguments of the selected sum, product, or equation.
\nThis command swaps the arguments of the selected sum, product,
or equation.  The sum ‘a + b + c’ is treated as the nested sums
‘(a + b) + c’ by this command.

If you put the cursor on the first ‘+’, the result is ‘(b + a) + c’;
if you put the cursor on the second ‘+’, the result is ‘c + (a + b)’

This function is a wrapper over `calc-sel-commute'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-commute'"
  (interactive)
  (call-interactively #'calc-sel-commute))

(defun casual-calc--sel-evaluate ()
  "Perform basic simplifications on the selected sub-formula.
\nThis function is a wrapper over `calc-sel-evaluate'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-evaluate'"
  (interactive)
  (call-interactively #'calc-sel-evaluate))

(defun casual-calc--sel-invert ()
  "Take the reciprocal of the selected term.
\nThis function is a wrapper over `calc-sel-invert'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-invert'"
  (interactive)
  (call-interactively #'calc-sel-invert))

(defun casual-calc--sel-isolate ()
  "Isolate the selected term on its side of an equation.
\nThis function is a wrapper over `calc-sel-isolate'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-isolate'"
  (interactive)
  (call-interactively #'calc-sel-isolate))

(defun casual-calc--sel-jump-equals ()
  "Move the selected term from one side of the equation to the other.
\nThis function is a wrapper over `calc-sel-jump-equals'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-jump-equals'"
  (interactive)
  (call-interactively #'calc-sel-jump-equals))

(defun casual-calc--sel-merge ()
  "Merge selected term.
\nThis can be viewed as the complement to
`casual-calc--sel-distribute'.
\nFor example, if given an expression ‘a b - a c’ with either ‘a b’
or ‘a c’ selected, the result is ‘a * (b - c)’.

This function is a wrapper over `calc-sel-merge'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-merge'"
  (interactive)
  (call-interactively #'calc-sel-merge))

(defun casual-calc--sel-negate ()
  "Replace selected term with the negative of that term, preserving equality.
\nThis command replaces the selected term with the negative of
that term, then adjusts the surrounding formula in order to
preserve equality to the formula's previous form.

This function is a wrapper over `calc-sel-negate'.

* References
- info node `(calc) Rearranging with Selections'
- `calc-sel-negate'"
  (interactive)
  (call-interactively #'calc-sel-negate))

(defun casual-calc--select-here ()
  "Select a sub-formula at cursor (point).
\nGiven a cursor located in an expression (or formula), this
command will highlight the smallest portion of that formula.
\nBy default the sub-formula is highlighted by blanking out all of
the rest of the formula with dots.

This function is a wrapper over `calc-select-here'.

* References
- info node `(calc) Making Selections'
- `calc-select-here'"
  (interactive)
  (call-interactively #'calc-select-here))

(defun casual-calc--select-less ()
  "Reduce the current selection around the point (cursor) position.
\nTypically use this command in conjunction with
`casual-calc--select-here'. \nThis function is a wrapper
over `calc-select-less'.

* References
- info node `(calc) Changing Selections'
- `calc-select-less'"
  (interactive)
  (call-interactively #'calc-select-less))

(defun casual-calc--select-more ()
  "Expand the current selection around the point (cursor) position.

Typically use this command in conjunction with
`casual-calc--select-here'.

This function is a wrapper over `calc-select-more'.

* References
- info node `(calc) Changing Selections'
- `calc-select-more'"
  (interactive)
  (call-interactively #'calc-select-more))

(defun casual-calc--select-next ()
  "Change the current selection to the next sub-formula at the same level.

Typically use this command in conjunction with
`casual-calc--select-here'.

This function is a wrapper over `calc-select-next'.

* References
- info node `(calc) Changing Selections'
- `calc-select-next'"
  (interactive)
  (call-interactively #'calc-select-next))

(defun casual-calc--select-once ()
  "Select a sub-formula such that selection lives only for the next command.
\nThis function is a wrapper over `calc-select-once'.

* References
- info node `(calc) Making Selections'
- `calc-select-once'"
  (interactive)
  (call-interactively #'calc-select-once))

(defun casual-calc--select-previous ()
  "Change the current selection to the previous sub-formula at the same level.

Typically use this command in conjunction with
`casual-calc--select-here'.

This function is a wrapper over `calc-select-previous'.

* References
- info node `(calc) Changing Selections'
- `calc-select-previous'"
  (interactive)
  (call-interactively #'calc-select-previous))

(defun casual-calc--solve-for ()
  "Rearrange an equation to solve for a specific variable.
\nAn equation is an expression of the form ‘L = R’.  This command
will prompt for a variable to solve for.
\nFor example if the equation is ‘y = 3x + 6’, the result will be ‘x = y/3 - 2’.
\nIf the input is not an equation, it is treated like an equation
of the form ‘X = 0’.
\nStack Arguments:
1: equation

This function is a wrapper over `calc-solve-for'.

* References
- info node `(calc) Solving Equations'
- `calc-solve-for'"
  (interactive)
  (call-interactively #'calc-solve-for))

(defun casual-calc--substitute ()
  "Substitute a variable or sub-expression of a formula with another.
\nSubstitutes all occurrences of some variable or sub-expression
of an expression with a new sub-expression.
\nStack Arguments:
1: n

This function is a wrapper over `calc-substitute'.

* References
- info node `(calc) Algebraic Manipulation'
- `calc-substitute'"
  (interactive)
  (call-interactively #'calc-substitute))

(defun casual-calc--summation ()
  "Computes the sum of a formula over a certain range of index values.
\nThe formula is taken from the top of the stack; the command
prompts for the name of the summation index variable, the lower
limit of the sum (any formula), and the upper limit of the sum.
\nStack Arguments:
1: formula

This function is a wrapper over `calc-summation'.

* References
- info node `(calc) Summations'
- `calc-summation'"
  (interactive)
  (call-interactively #'calc-summation))

(defun casual-calc--tabulate ()
  "Evaluate a formula as a series of iterated index values.
\nThe formula is taken from the top of the stack; the command
prompts for the name of the summation index variable, the lower
limit of the sum (any formula), and the upper limit of the sum.

\nStack Arguments:
1: formula

This function is a wrapper over `calc-tabulate'.

* References
- info node `(calc) Summations'
- `calc-tabulate'"
  (interactive)
  (call-interactively #'calc-tabulate))

(defun casual-calc--taylor ()
  "Computes a power series expansion or Taylor series of a function.
\nThis command prompts you for a variable and a desired number of terms.
You may give an expression of the form ‘VAR = A’ or ‘VAR - A’
instead of just a variable to produce a Taylor expansion about
the point A.  Note that many series expansions have
coefficients of zero for some terms, so you may appear to get
fewer terms than you asked for.
\nStack Argument:
1: equation

This function is a wrapper over `calc-taylor'.

* References
- info node `(calc) Taylor Series'
- `calc-taylor'"
  (interactive)
  (call-interactively #'calc-taylor))

(defun casual-calc--unselect ()
  "Unselects the formula that the cursor is on.
\nIf there was no selection in the formula, this command
has no effect.

This function is a wrapper over `calc-unselect'.

* References
- info node `(calc) Making Selections'
- `calc-unselect'"
  (interactive)
  (call-interactively #'calc-unselect))

(provide 'casual-calc-algebra)
;;; casual-calc-algebra.el ends here
