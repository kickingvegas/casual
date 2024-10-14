;;; casual-calc-financial.el --- Casual Financial Menu    -*- lexical-binding: t; -*-

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
(require 'calc-ext)
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-stack)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-financial-tmenu ()
  "Casual financial menu.
\nTop-level menu of financial functions."
  [["Compound Interest"
    ("f" "Future/Present Value›" casual-calc-fin-pv-fv-tmenu)
    ("p" "Periodic Payment›" casual-calc-fin-periodic-payments-tmenu)
    ("n" "# of Payments›" casual-calc-fin-number-of-payments-tmenu)
    ("t" "# of Periods To Reach Target›" casual-calc-fin-periods-to-target-tmenu)
    ("r" "Rate of Return›" casual-calc-fin-rate-of-return-tmenu)]

   ["Percentages"
    ("%" "%" calc-percent :transient t)
    ("c" "→%" calc-convert-percent :transient t)
    ("D" "Δ%" calc-percent-change :transient t)
    ("=" "=" calc-evaluate :transient t)]

   ["Display"
    ("M-s" "Stack" casual-calc-stack-display-tmenu)
    ("R" "Refresh" calc-refresh :transient t)
    ("L" "Last" calc-last-args :transient t)]

   casual-calc-basic-operators-group]

  ["Discounted Cash Flow Analysis"
   ("v" "Net Present Value›" casual-calc-fin-npv-tmenu)
   ("i" "Internal Rate of Return›" casual-calc-fin-irr-tmenu)]

  ["Depreciation"
   ("d" "Depreciation›" casual-calc-fin-depreciation-tmenu)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-npv-tmenu ()
  "Casual menu to calculate net present value.
\nProvides menu argument support for the following function:
- `casual-calc--fin-npv'"
  [["Net Present Value with Irregular Deposits"
   :class transient-row
   ("r" "Interest Rate (%)" "--rate=" :prompt "Interest Rate: ")
   ("b" "Beginning" "--beginning")]
   casual-calc-basic-operators-group]

  [("n" "Net Present Value (1:)" casual-calc--fin-npv :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-pv-fv-tmenu ()
  "Casual menu for periodic present or future value functions.
\nMenu with argument support for the following functions:
- `casual-calc--fin-fv-periodic'
- `casual-calc--fin-fv-lump'
- `casual-calc--fin-pv-periodic'
- `casual-calc--fin-pv-lump'"
  :value '("--rate=5.4" "--periods=5" "--amount=1000")

  ["Future/Present Value"
   [("r" "Interest Rate (%)" "--rate=" :prompt "Interest Rate: ")
    ("n" "Number of Periods" "--periods=" :prompt "Number of Time Periods: ")]
   [("a" "Amount" "--amount=" :prompt "Amount: ")
    ("b" "Beginning" "--beginning")]
   casual-calc-basic-operators-group]

  ["Future Value"
   ("f" "Periodic" casual-calc--fin-fv-periodic :transient t)
   ("F" "Lump Sum" casual-calc--fin-fv-lump :transient t)]

  ["Present Value"
    ("p" "Periodic" casual-calc--fin-pv-periodic :transient t)
    ("P" "Lump Sum" casual-calc--fin-pv-lump :transient t)]

  casual-calc-navigation-group)



(transient-define-prefix casual-calc-fin-periodic-payments-tmenu ()
  "Casual periodic payments menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-periodic-payment'"
  ["Periodic Payments"
   ["Parameters"
    ("a" "Loan Amount" "--amount=" :prompt "Loan Amount: ")
    ("r" "Interest Rate per Period (in %)" "--rate=" :prompt "Interest Rate (%): ")
    ("n" "Number of Periods" "--periods=" :prompt "Number of periods in loan: ")
    ("b" "Beginning" "--beginning")]
   casual-calc-basic-operators-group]

  ["Projected payment per period"
   ("p" "Payment" casual-calc--fin-periodic-payment :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-number-of-payments-tmenu ()
  "Casual periodic payments menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-number-of-payments'"
  ["Number of Payments"
   ["Parameters"
    ("a" "Loan Amount" "--amount=" :prompt "Loan Amount: ")
    ("r" "Interest Rate per Period (in %)" "--rate=" :prompt "Interest Rate (%): ")
    ("p" "Payment" "--payment=" :prompt "Payment: ")
    ("b" "Beginning" "--beginning")]
   casual-calc-basic-operators-group]

  ["Projected number of payments"
    ("n" "Number of Payments" casual-calc--fin-number-of-payments :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-rate-of-return-tmenu ()
  "Casual rate of return menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-rate-of-return'"
  ["Compute Rate of Return on Present Value"
   ["Parameters"
    :class transient-row
    ("n" "periods" "--periods=" :prompt "Number of Periods: ")
    ("p" "payment" "--payment=" :prompt "Payment: ")
    ("v" "present value" "--pv=" :prompt "Present value: ")]
   casual-calc-basic-operators-group]
  ;; TODO: note no --beginning argument because I `calc-fin-rate' is broken.

  ["Rate of Return"
   ("r" "Where payment made at end of period" casual-calc--fin-rate-of-return :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-irr-tmenu ()
  "Casual internal rate of return menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-irr'"
  ["Internal Rate of Return for Irregular Payments"
   ["Parameters"
    ("b" "Beginning" "--beginning")]
   casual-calc-basic-operators-group]

  [("i" "Internal Rate of Return (1:)" casual-calc--fin-irr :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-periods-to-target-tmenu ()
  "Casual periods to target menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-periods-to-reach-target'"
  ["Number of Periods to Reach Investment Target"
   ["Parameters"
    :class transient-row
    ("t" "target" "--target=" :prompt "target amount: ")
    ("r" "rate" "--rate=" :prompt "rate (%): ")
    ("d" "deposit" "--deposit=" :prompt "deposit: ")]
   casual-calc-basic-operators-group]

  ["Periods to Target"
   ("n" "Periods" casual-calc--fin-periods-to-reach-target :transient t)]

  casual-calc-navigation-group)

(transient-define-prefix casual-calc-fin-depreciation-tmenu ()
  "Casual depreciation menu.
\nMenu with argument support for the following functions:
- `casual-calc--fin-depreciation-straight-line'
- `casual-calc--fin-depreciation-sum-of-years'
- `casual-calc--fin-depreciation-double-declining-balance'"
  :value '("--period=1")

  ["Parameters"
   [("c" "cost" "--cost=" :prompt "cost: ")
    ("s" "salvage" "--salvage=" :prompt "salvage: ")]
   [("l" "life" "--life=" :prompt "life: ")
    ("p" "period" "--period=" :prompt "period (1..life)): ")]
   casual-calc-basic-operators-group]

  ["Depreciation"
   ("1" "Straight Line (c, s, l)" casual-calc--fin-depreciation-straight-line :transient t)
   ("2" "Sum-of-Years Digits (c, s, l, p)" casual-calc--fin-depreciation-sum-of-years :transient t)
   ("3" "Double Declining Balance (c, s, l, p)" casual-calc--fin-depreciation-double-declining-balance :transient t)]

  casual-calc-navigation-group)

;; Wrapped Functions -----

(defun casual-calc--fin-npv ()
  "Net present value of irregular investments.
\nCompute the net present value of a series of irregular investments.
\nMenu argument:\n
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nThis command requires the stack to be populated with the argument:\n
1: vector of irregular investments

* References
- info node `(calc) Present Value'
- `calc-fin-npv'"
  (interactive)
  (let ((rate (math-read-number-simple
               (transient-arg-value
                "--rate="
                (transient-args transient-current-command))))

        (beginning (transient-arg-value
                    "--beginning"
                    (transient-args transient-current-command))))

    (calc-push rate)
    (calc-percent)
    (call-interactively #'calc-roll-down)
    (if beginning
        (calc-inverse))
    (call-interactively #'calc-fin-npv)))

(defun casual-calc--fin-irr ()
  "Internal rate of return for irregular payments.
\nThe internal rate of return is defined as the discount rate
that results in a net present value (NPV) of zero. Internal rate
of return analysis is a financial technique that is often used to
assess the likely profitability of a project or investment.
\nIRR assumes that cash flows occur regularly, in equal length time
periods. Cash flow values may vary between periods and represent
either income (positive) or expenditure (negative). At least one
value must be negative and at least one value must be positive.
\n\nMenu argument:\n
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nThis command requires the stack to be populated with the argument:\n
1: vector of irregular payments

* References
- info node `(calc) Related Financial Functions'
- URL `https://wiki.documentfoundation.org/Documentation/Calc_Functions/IRR'
- `calc-fin-irr'"
  (interactive)
  (if (transient-arg-value "--beginning" (transient-args transient-current-command))
      (calc-inverse))
  (call-interactively #'calc-fin-irr)
  (calc-convert-percent))

(defun casual-calc--fin-fv-periodic ()
  "Future value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit amount per time period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Future Value'.
- `calc-fin-fv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "fvb" (cons #'calcFunc-fvb (calc-top-list-n 3)))
       (calc-enter-result 3 "fv" (cons #'calcFunc-fv (calc-top-list-n 3)))))))

(defun casual-calc--fin-pv-periodic ()
  "Present value given deposits over time.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods to invest over
--amount=a  : deposit amount per time period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Present Value'.
- `calc-fin-pv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "pvb" (cons #'calcFunc-pvb (calc-top-list-n 3)))
       (calc-enter-result 3 "pv" (cons #'calcFunc-pv (calc-top-list-n 3)))))))

(defun casual-calc--fin-fv-lump ()
  "Future value, lump sum.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : deposit amount
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Future Value'.
- `calc-fin-fv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command))))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (calc-enter-result 3 "fvl" (cons #'calcFunc-fvl (calc-top-list-n 3))))))

(defun casual-calc--fin-pv-lump ()
  "Present value, lump sum.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : deposit amount
\nSetting the above values loads the stack accordingly:
3: r
2: n
1: a

* References
- info node `(calc) Present Value'
- `calc-fin-pv'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command))))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (calc-enter-result 3 "pvl" (cons #'calcFunc-pvl (calc-top-list-n 3))))))

(defun casual-calc--fin-periodic-payment ()
  "Amount of periodic payment necessary to amortize a loan.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--rate=r    : % interest rate per period of time
--periods=n : number of time periods for investment
--amount=a  : loan amount
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-pmt'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push periods)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "pmtb" (cons #'calcFunc-pmtb (calc-top-list-n 3)))
       (calc-enter-result 3 "pmt" (cons #'calcFunc-pmt (calc-top-list-n 3)))))))

(defun casual-calc--fin-number-of-payments ()
  "Number of regular payments to amortize a loan.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--amount=a  : loan amount
--rate=r    : % interest rate per period of time
--payment=p : payment per period
--beginning : if set then calculate where payment is made at
              the start of a period, otherwise end.
\nSetting the above values loads the stack accordingly:
3: r
2: p
1: a

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-nper'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (payment (math-read-number-simple
                   (transient-arg-value "--payment=" current-command)))
         (amount (math-read-number-simple
                  (transient-arg-value "--amount=" current-command)))
         (beginning (transient-arg-value "--beginning" current-command)))

    (calc-push rate)
    (calc-percent)
    (calc-push payment)
    (calc-push amount)

    (calc-slow-wrapper
     (if beginning
         (calc-enter-result 3 "nprb" (cons #'calcFunc-nperb (calc-top-list-n 3)))
       (calc-enter-result 3 "nper" (cons #'calcFunc-nper (calc-top-list-n 3)))))))

(defun casual-calc--fin-periods-to-reach-target ()
  "Number of periods to reach an investment target.
\nThis command (suffix) is called from a Transient menu.
\nMenu arguments:\n
--target=t  : investment target amount
--rate=r    : interest rate per period
--deposit=d : deposit amount per period

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-nper'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (rate (math-read-number-simple
                (transient-arg-value "--rate=" current-command)))
         (target (math-read-number-simple
                  (transient-arg-value "--target=" current-command)))
         (deposit (math-read-number-simple
                   (transient-arg-value "--deposit=" current-command))))
    (calc-push rate)
    (calc-percent)
    (calc-push target)
    (calc-push deposit)

    (calc-slow-wrapper
     (calc-enter-result 3 "nprl" (cons #'calcFunc-nperl (calc-top-list-n 3))))))

(defun casual-calc--fin-rate-of-return ()
  "Compute rate of return on investment.
\nMenu arguments:\n
--periods=n    : number of periods
--payment=p    : payment per period
--pv=v         : present value of asset

* References
- info node `(calc) Related Financial Functions'
- `calc-fin-rate'"
  (interactive)
  (let* ((current-command (transient-args transient-current-command))
         (periods (math-read-number-simple
                   (transient-arg-value "--periods=" current-command)))
         (payment (math-read-number-simple
                   (transient-arg-value "--payment=" current-command)))
         (pv (math-read-number-simple
              (transient-arg-value "--pv=" current-command))))
    (calc-push periods)
    (calc-push payment)
    (calc-push pv)

    ;; TODO: inverse calc-fin-rate is broken
    (calc-slow-wrapper
     (calc-pop-push-record 3
                           "rate"
                           (calc-to-percentage
                            (calc-normalize
                             (cons #'calcFunc-rate (calc-top-list-n 3))))))))

(defun casual-calc--fin-depreciation-straight-line ()
  "Compute depreciation using “straight-line” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-sln'"
  (interactive)
  (casual-calc--fin-process-depreciation #'calc-fin-sln))

(defun casual-calc--fin-depreciation-sum-of-years ()
  "Compute depreciation using “sum-of-years-digits” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life
--period=p  : specific period (year) of life, must be within [1..l]

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-syd'"
  (interactive)
  (casual-calc--fin-process-depreciation #'calc-fin-syd))

(defun casual-calc--fin-depreciation-double-declining-balance ()
  "Compute depreciation using “double-declining balance” method.
\nComputes the amount of value lost over time.
\nMenu arguments:\n
--cost=c    : original cost of asset
--salvage=s : salvage value of asset at end of life
--life=l    : number of periods (typically years) of asset life
--period=p  : specific period (year) of life, must be within [1..l]

* References
- info node `(calc) Depreciation Functions'
- `calc-fin-ddb'"
  (interactive)
  (casual-calc--fin-process-depreciation #'calc-fin-ddb))

(defun casual-calc--fin-process-depreciation (fn)
  "Process financial prefix with MODIFIER, FN.
\nProcess Transient prefix arguments and invoke function using said arguments.
FN: calc function to invoke"
  (let* ((arg1 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "cost")
                 (transient-args transient-current-command))))
         (arg2 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "salvage")
                 (transient-args transient-current-command))))
         (arg3 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "life")
                 (transient-args transient-current-command))))
         (arg4 (math-read-number-simple
                (transient-arg-value
                 (format "--%s=" "period")
                 (transient-args transient-current-command)))))

    (calc-push arg1)
    (calc-push arg2)
    (calc-push arg3)
    (if (not (eq fn #'calc-fin-sln))
        (calc-push arg4))

    (funcall fn)))

(provide 'casual-calc-financial)
;;; casual-calc-financial.el ends here
