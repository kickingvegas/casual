;;; casual-lib-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'kmacro)

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
  (defun casualt-stub (&rest _)
    (number-to-register value ?9))

  (print (format "testing %s binding: %s for %s" menu binding cmd))
  (advice-add cmd :override #'casualt-stub)
  (funcall-interactively menu)
  (execute-kbd-macro binding)
  (should (equal value (get-register ?9)))
  (advice-remove cmd #'casualt-stub))

;; TODO: obsolete
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
          (let ((binding (alist-get :binding x))
                (command (alist-get :command x)))
            (casualt-testbench-transient-suffix menu
                                                binding
                                                command
                                                (funcall value-fn))))
        test-vectors))

;; TODO: obsolete
(defun casualt-suffix-test-vector (binding command &optional argcount)
  "Create suffix test vector given BINDING, COMMAND, and ARGCOUNT."
  (let ((argcount (or argcount 0))
        (result (list)))
    (push (cons :binding binding) result)
    (push (cons :command command) result)
    (push (cons :argcount argcount) result)
    result))

(defun casualt-suffix-testcase-runner (testcases menu value-fn)
  "Test runner for suffixes in MENU specified in TESTCASES testing VALUE-FN.
This function executes `casualt-testbench-transient-suffix' for all elements
in TESTCASES.

TESTCASES - list of plists with keys :binding, :command
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
          (let ((binding (kbd (plist-get x :binding)))
                (command (plist-get x :command)))
            (casualt-testbench-transient-suffix menu
                                                binding
                                                command
                                                (funcall value-fn))))
        testcases))

(defmacro casualt-mock (fn)
  "Substitute interactive function FN with a mock function.

This macro is intended to be used in a `cl-letf' call to mock
functions that can not be advised."
  (list '((symbol-function fn) (lambda (x) (interactive)(print "WARNING: override")))))

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

(defun casualt-unicode-db-assert (key control cmd)
  "DB Assert."
  (let ((test (funcall cmd key)))
    (should (string= test control))))


(provide 'casual-lib-test-utils)
;;; casual-lib-test-utils.el ends here
