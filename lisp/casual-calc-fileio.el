;;; casual-calc-fileio.el --- Casual File I/O Routines    -*- lexical-binding: t; -*-

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
(require 'calc-vec)
(require 'calc-ext)



(defun casual-calc-datafile-to-matrix (filename)
  "Read data in FILENAME into a matrix list.

This command reads a data file into a matrix list that can be
processed by Calc.

This matrix list is an Elisp list of lists where each list
element holds a value from a line from the data file. The
elements of this matrix list correspond in order to each
successive line in the data file. The general form of this list
is: ((a0 b0 c0 …) (a1 b1 c1 …) (a2 b2 c2 …) …), where a𝑛, b𝑛, c𝑛
are the data values in a line (or row) and 𝑛 is the row index in
the data file. The type of a𝑛, b𝑛, c𝑛 are determined by the Calc
function `math-read-number-simple'.

Data in the file are separated using the following regexp: \"[ \\f\\t\\v,]+\"
This regexp accepts the following separators:
- space
- tab
- comma
- formfeed character
- vertical tab"
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((templist (list)))
      (while (not (eobp))
        (let ((start (point)))
          (move-end-of-line nil)
          (let ((line (buffer-substring start (point))))
            (cond
             ((not (string-equal line ""))
              (push
               (mapcar #'math-read-number-simple
                       (split-string (buffer-substring start (point)) "[ \f\t\v,]+"))
               templist)
              (if (not (eobp)) (forward-char)))))))
      (reverse templist))))

;; math-read-number-simple
;; string-to-number

;;(kill-new (pp (casual-calc-datafile-to-matrix "~/fred.dat")))

(defun casual-calc-matrix-to-calc-vector (matrix)
  "Convert matrix into a Calc vector MATRIX.

This function converts the output of `casual-calc-datafile-to-matrix'
to a Calc matrix."
  (let ((templist (list)))
    (mapc (lambda (x)
            (push (apply #'calcFunc-vec x) templist))
          matrix)
    (apply #'calcFunc-vec (reverse templist))))

(defun casual-calc-read-curvefit-data (filename)
  "Read data into FILENAME for curve fitting.

This command reads a data file for curve fitting, pushing its
contents on the stack in a form that is consumable by
`casual-calc--curve-fit'.

Data in the file are separated using the following regexp: \"[ \\f\\t\\v,]+\"
This regexp accepts the following separators:
- space
- tab
- comma
- formfeed character
- vertical tab

* References
- `casual-calc--curve-fit'"
  (interactive "fCurvefit Data File: ")
  (calc-transpose
   (calc-push
    (casual-calc-matrix-to-calc-vector
     (casual-calc-datafile-to-matrix filename)))))

(defun casual-calc-read-plot-data (filename)
  "Read data into FILENAME for curve plotting.

This command reads a data file for curve plotting, pushing its
contents on the stack in a form that is consumable for plotting.
Depending on whether the data file has two or three columns, this
data can be plotted using either `casual-calc--graph-add' or
`casual-calc--graph-add-3d'."
  (interactive "fPlot Data File: ")
  (calc-unpack
   (calc-transpose
    (calc-push
     (casual-calc-matrix-to-calc-vector
      (casual-calc-datafile-to-matrix filename))))))

(provide 'casual-calc-fileio)
;;; casual-calc-fileio.el ends here
