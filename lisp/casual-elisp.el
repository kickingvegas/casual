;;; casual-elisp.el --- Transient UI for Elisp Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Choi

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

;; This library provides a Transient-based user interface for `emacs-lisp-mode'.

;; INSTALL

;; In your initialization file, bind the Transient `casual-elisp-tmenu' to
;; your key binding of preference.

;; (keymap-set emacs-lisp-mode-map "M-m" #'casual-elisp-tmenu)

;;; Code:

(require 'xref)
(require 'find-func)
(require 'casual-elisp-settings)
(require 'casual-elisp-utils)

;;;###autoload (autoload 'casual-elisp-tmenu "casual-elisp" nil t)
(transient-define-prefix casual-elisp-tmenu ()
  ["Casual Elisp"
   :description (lambda () (format "Casual Elisp: %s" (buffer-name)))
   ["Evaluate"
    ("x" "Last Sexp" eval-last-sexp)
    ("L" "Buffer or Region" elisp-eval-region-or-buffer
     :description (lambda () (if (use-region-p) "Region" "Buffer")))
    ("d" "Defun" eval-defun
     :description (lambda () (if prefix-arg "Edebug" "Defun✦")))]

   ["Xref"
    ("." "Definition" xref-find-definitions)
    ("r" "References" xref-find-references)
    ("R" "Replace" xref-find-references-and-replace)]

   ["Checkdoc"
    ("c" "Checkdoc" checkdoc)]

   ["Byte-Compile"
    ("B" "File" elisp-byte-compile-file)
    ("b" "Buffer" elisp-byte-compile-buffer)
    ("D" "Directory…" byte-recompile-directory)]

   ["Find"
    ("l" "Library…" find-library)
    ("v" "Variable…" find-variable)
    ("f" "Function…" find-function)]]

  ["Navigate"
   :pad-keys t
   ["Backward"
    :pad-keys t
    ("<left>" "Char" backward-char
     :description (lambda () (casual-elisp-unicode-get :backward-char))
     :transient t)
    ("C-<left>" "Sexp" backward-sexp
     :description (lambda () (casual-elisp-unicode-get :backward-sexp))
     :transient t)] ; "(←"
   ["Forward"
    :pad-keys t
    ("<right>" "Char" forward-char
     :description (lambda () (casual-elisp-unicode-get :forward-char))
     :transient t)
    ("C-<right>" "Sexp" casual-elisp-next-sexp
     :description (lambda () (casual-elisp-unicode-get :forward-sexp))
     :transient t)]
   ["Up"
    :pad-keys t
    ("<up>" "Line" previous-line
     :description (lambda () (casual-elisp-unicode-get :previous-line))
     :transient t)
    ("C-<up>" "List" backward-up-list
     :description (lambda () (casual-elisp-unicode-get :backward-up-list))
     :transient t)] ; "(↰"
   ["Down"
    :pad-keys t
    ("<down>" "Line" next-line
     :description (lambda () (casual-elisp-unicode-get :next-line))
     :transient t)
    ("C-<down>" "List" down-list
     :description (lambda () (casual-elisp-unicode-get :down-list))
     :transient t)]] ; "⤵("

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings›" casual-elisp-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])

(provide 'casual-elisp)
;;; casual-elisp.el ends here
