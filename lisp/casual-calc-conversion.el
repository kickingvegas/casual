;;; casual-calc-conversion.el --- Casual Conversion Menu  -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'casual-lib)
(require 'casual-calc-utils)

(transient-define-prefix casual-calc-conversions-tmenu ()
  "Casual conversion functions menu."
  ["Conversions"

   ["Angle"
    ("d" "To Degrees" calc-to-degrees
     :description (lambda ()
                    (format "%s %s %s"
                            (casual-calc-unicode-get :radians)
                            (casual-calc-unicode-get :to)
                            (casual-calc-unicode-get :degrees)))
     :transient t)
    ("r" "To Radians" calc-to-radians
     :description (lambda ()
                    (format "%s %s %s"
                            (casual-calc-unicode-get :degrees)
                            (casual-calc-unicode-get :to)
                            (casual-calc-unicode-get :radians)))
     :transient t)]

    ["HMS"
     ("h" "To ℎ𝑚𝑠" calc-to-hms
      :description (lambda ()
                    (format "real %s %s"
                            (casual-calc-unicode-get :to)
                            (casual-calc-unicode-get :hms)))
      :transient t)
     ("H" "From ℎ𝑚𝑠" calc-from-hms
      :description (lambda ()
                     (format "%s %s real"
                             (casual-calc-unicode-get :hms)
                             (casual-calc-unicode-get :to)))
      :transient t)]

    ["Numeric"
     ("f" "To Float" calc-float
      :description (lambda ()
                     (format "%s %s"
                             (casual-calc-unicode-get :to)
                             (casual-calc-unicode-get :float)))
      :transient t)
     ("F" "To Fraction" calc-fraction
      :description (lambda ()
                     (format "%s %s"
                             (casual-calc-unicode-get :to)
                             (casual-calc-unicode-get :fraction)))
      :transient t)]]

  casual-calc-operators-group-row

  casual-calc-navigation-group)

(provide 'casual-calc-conversion)
;;; casual-calc-conversion.el ends here
