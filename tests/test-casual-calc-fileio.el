;;; test-casual-calc-fileio.el --- Casual File I/O Tests  -*- lexical-binding: t; -*-

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
(require 'ert)
(require 'casual-calc-test-utils)
(require 'casual-calc-fileio)

(defun casualt-write-temp-datafile (filename data separator)
  "Write DATA to FILENAME separating columns by SEPARATOR."
  (let ((buflist (list)))
    (mapc
     (lambda (x)
       (push (string-join (mapcar #'number-to-string x) separator) buflist))
     data)

    (with-temp-file filename
      (insert (string-join (reverse buflist) "\n")))))


(ert-deftest test-casual-calc-read-curvefit-data ()
  (casualt-calc-setup)

  (let ((filename (make-temp-name "/tmp/casualt-"))
        (input '((1.0 7.5) (3.3 5.9) (120.0 92.1)))
        (separators '(" " "\t" "," "\f" "\v"))
        (control '(vec
                   (vec
                    (float 1 0)
                    (float 33 -1)
                    (float 12 1))
                   (vec
                    (float 75 -1)
                    (float 59 -1)
                    (float 921 -1)))))

    (mapc (lambda (sep)
            (casualt-write-temp-datafile filename input sep)
            (casual-calc-read-curvefit-data filename)
            (should (equal (calc-top) control))
            (delete-file filename))
          separators))

  (casualt-calc-breakdown t))

(ert-deftest test-casual-calc-read-plot-data ()
  (casualt-calc-setup)

  (let ((filename (make-temp-name "/tmp/casualt-"))
        (input '((1.0 7.5) (3.3 5.9) (120.0 92.1)))
        (separators '(" " "\t" "," "\f" "\v"))
        (control '(vec
                   (vec
                    (float 1 0)
                    (float 33 -1)
                    (float 12 1))
                   (vec
                    (float 75 -1)
                    (float 59 -1)
                    (float 921 -1)))))

    (mapc (lambda (sep)
            (casualt-write-temp-datafile filename input sep)
            (casual-calc-read-plot-data filename)
            (should (equal (calc-top)
                           '(vec
                             (float 75 -1)
                             (float 59 -1)
                             (float 921 -1))))

            (should (equal (calc-top-n 2)
                           '(vec
                             (float 1 0)
                             (float 33 -1)
                             (float 12 1))))

            (delete-file filename))
          separators))

  (casualt-calc-breakdown t))
(provide 'test-casual-calc-fileio)
;;; test-casual-calc-fileio.el ends here
