;;; casual-calc-variables.el --- Casual Variable Menu     -*- lexical-binding: t; -*-

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
(require 'casual-calc--calc)
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-labels)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-variable-crud-tmenu ()
  "Stored variable operations menu.
Operations to store, recall, clear, and edit variables are provided by this
menu."
  ["Variable Operations"
   ("s" "Store (𝟣:)…" casual-calc--store :transient t)
   ("r" "Recall…" casual-calc--recall :transient t)
   ("c" "Clear…" casual-calc--unstore :transient t)
   ("e" "Edit…" casual-calc--edit-variable :transient nil)
   ("o" "Copy to other variable…" casual-calc--copy-variable :transient t)
   ("x" "Exchange (𝟣:) to variable…" casual-calc--store-exchange :transient t)
   ("p" "Persist…" casual-calc--permanent-variable :transient t)
   ("O" "Open Calc Settings File" casual-calc-open-settings-file :transient nil)
   ("i" "Insert variables into buffer…" casual-calc--insert-variables :transient t)]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-calc-algebraic-entry)
          (casual-calc-pop)
          (casual-calc-undo-suffix)
          (casual-lib-quit-all)])

(provide 'casual-calc-variables)
;;; casual-calc-variables.el ends here
