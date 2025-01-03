;;; casual-calc-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'ert)
(require 'calc)
(require 'transient)
(require 'kmacro)

(defvar casualt-test-operators-group
  '(("+" . casual-calc--plus)
    ("-" . casual-calc--minus)
    ("*" . casual-calc--times)
    ("/" . casual-calc--divide)
    ("%" . casual-calc--mod)))

(defvar casualt-test-basic-operators-group
  '(("+" . casual-calc--plus)
    ("-" . casual-calc--minus)
    ("*" . casual-calc--times)
    ("/" . casual-calc--divide)))

(defvar casualt-test-navigation-group
  '(("U" . calc-undo)
    ("`" . calc-algebraic-entry)
    ("" . calc-pop)
    ("" . calc-enter)))

(defun casualt-calc-setup ()
  "Casual menu test setup function."
  (calc-create-buffer))

(defun casualt-calc-breakdown (&optional clear dismiss)
  "Casual menu test breakdown function, if CLEAR is non-nil then clear stack."
  (if clear
      (calc-pop-stack (calc-stack-size)))
  (if dismiss
      (execute-kbd-macro ""))
  (calc-quit nil))

(defun casualt-menu-input-testcase (menu keyseq init value)
  "Testcase that compares VALUE with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
VALUE - value to be compared with top of the stack"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (should (equal (calc-top) value)))

(defun casualt-menu-assert-testcase (menu keyseq init assert)
  "Testcase to call ASSERT with stack result from entering KEYSEQ on MENU.
MENU - Transient menu
KEYSEQ - key sequence that is bound to a menu item
INIT - list of arguments to be pushed on the Calc stack before running KEYSEQ
ASSERT - assert function that calls `should'"
  (if (and init (listp init))
      (calc-push-list init))
  (funcall menu)
  (execute-kbd-macro keyseq)
  (funcall assert))

(defun casualt-run-menu-input-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instancesg\n
A testcase is a list with the following schema - (keyseq init value)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
value - value to be compared with top of the stack\n
A testcase is used as input to `casualt-menu-input-testcase'."
  (mapc (lambda (x)
          (casualt-menu-input-testcase menu
                                       (car x)
                                       (nth 1 x)
                                       (nth 2 x)))
        testcases))

(defun casualt-run-menu-assert-testcases (menu testcases)
  "Test runner to exercise MENU with input TESTCASES.
MENU - Transient menu
TESTCASES - list of testcase instances\n
A testcase is a list with the following schema - (keyseq init assert)
keyseq - key sequence that is bound to a menu item
init - list of arguments to be pushed on the Calc stack before running keyseq
assert - assert function that calls `should'\n
A testcase is used as input to `casualt-menu-assert-testcase'."
  (mapc (lambda (x)
          (casualt-menu-assert-testcase menu
                                        (car x)
                                        (nth 1 x)
                                        (nth 2 x)))
        testcases))

(defun casualt-testbench-calc-fn (fn t-args value)
  "Calc function FN testbench using Transient arguments T-ARGS, testing VALUE.
FN - Calc function to test
T-ARGS - list of transient prefix argument values
VALUE - value to test against the top of the Calc stack"
  (defun casualt-mock-transient-args (prefix)
    "Mock function for transient-args with PREFIX argument."
    t-args)

  (advice-add #'transient-args :override  #'casualt-mock-transient-args)
  (call-interactively fn)
  (should (equal value (calc-top)))
  (advice-remove #'transient-args #'casualt-mock-transient-args))


(defun casualt-testbench-transient-suffix (menu binding cmd value)
  "Transient suffix testbench for BINDING and CMD in MENU testing VALUE.
This function is intended to test a Transient suffix binding
only. It does not exercise the actual command itself.

MENU - Transient prefix
BINDING - keybinding for suffix (menu item) to be tested
CMD - suffix (menu item) function to be overridden
VALUE - value to test against the top of the Calc stack

This function creates a testbench to exercise a menu item
command (Transient suffix) in a menu (prefix). The command
associated with that binding is overridden to instead push a
value to the top of the Calc stack. This value is then tested."
  (defun casualt-stub ()
    (calc-push value))
  (print (format "testing %s binding: %s for %s" menu binding cmd))
  (advice-add cmd :override #'casualt-stub)
  (funcall menu)
  (execute-kbd-macro binding)
  (should (equal value (calc-top)))
  (advice-remove cmd #'casualt-stub))

(defun casualt-suffix-testbench-runner (test-vectors menu value-fn)
  "Test runner for suffixes in MENU specified in TEST-VECTORS testing VALUE-FN.
This function executes `casualt-testbench-transient-suffix' for all elements
in TEST-VECTORS.

TEST-VECTORS - alist of (keysequence . command-function) elements
MENU - Transient prefix
VALUE-FN - function generator of value to test against on top of the Calc stack

An element in TEST-VECTOR consists of the following:

keysequence - a key sequence to be exercised by `execute-kbd-macro'
command-function - suffix command to be overridden

command-function is overridden to push the result of VALUE-FN
onto the top of the Calc stack.  This value is subsequently
compared to test that the binding is working as expected.

The value of keysequence is typically the keybinding value of the
command (suffix). However if the suffix does not dismiss the
Transient prefix that calls it, then the sequence should include
values which trigger dismissal of the prefix. An example would be
appending \"q\" to the keysequence."
  (mapc (lambda (x)
            (let ((key (car x))
                  (cmd (cdr x)))
              (casualt-testbench-transient-suffix menu
                                                  key
                                                  cmd
                                                  (funcall-interactively value-fn))))
          test-vectors))


(defun casualt-macro-callable-symbol (command)
  "Convert COMMAND to key string that can be passed into `kmacro'."
  (concat
   (seq-reduce 'concat
               (mapcar (lambda (c)
                         (concat (char-to-string c) " ") )
                       (symbol-name command))
               "M-x ")
   "<return> "))

(defun casualt-kmacro (command keys)
  "Create `kmacro' instance invoking COMMAND and passing KEYS to drive it.
COMMAND is an interactive function."
  (let ((buf (concat (casualt-macro-callable-symbol command) keys)))
    (kmacro buf)))

(provide 'casual-calc-test-utils)
;;; casual-calc-test-utils.el ends here
