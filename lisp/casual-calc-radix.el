;;; casual-calc-radix.el --- Casual Radix Menu            -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-radix-tmenu ()
  "Casual radix functions menu."
  ["Radix (𝑛 is 𝟣: on stack)"
   ("0" "Decimal" calc-decimal-radix :transient nil)
   ("2" "Binary" calc-binary-radix :transient nil)
   ("8" "Octal" calc-octal-radix :transient nil)
   ("6" "Hexadecimal" calc-hex-radix :transient nil)
   ("n" "Other base 𝑛" calc-radix :transient nil)]
  [:class transient-row
          (casual-lib-quit-one)
          (casual-calc-algebraic-entry)
          (casual-calc-pop)
          (casual-calc-undo-suffix)
          (casual-lib-quit-all)])

(provide 'casual-calc-radix)
;;; casual-calc-radix.el ends here
