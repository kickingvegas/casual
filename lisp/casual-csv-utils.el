;;; casual-csv-utils.el --- Casual CSV Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

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
(require 'csv-mode)
(require 'casual-lib)
(require 'org-table)

(defconst casual-csv-unicode-db
  '((:up . '("↑" "Up"))
    (:down . '("↓" "Down"))
    (:right . '("→" "Right"))
    (:left . '("←" "Left"))
    (:bol . '("⇤" "Begin"))
    (:eol . '("⇥" "End"))
    (:beginning-of-buffer . '("⇱" "Begin"))
    (:end-of-buffer . '("⇲" "End")))
  "Unicode symbol DB to use for CSV Transient menus.")

(defun casual-csv-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-csv-unicode-db))

(defun casual-csv-kill-region-as-org-table (start end)
  "Copy CSV region at START, END as Org table in the `kill-ring'."
  (interactive "r")
  (let ((buf (buffer-substring start end)))
    (with-temp-buffer
      (insert buf)
      (org-table-convert-region (point-min) (point-max))
      (kill-region (point-min) (point-max)))))

(defun casual-csv-align-auto ()
  "Auto align CSV fields."
  (interactive)
  (setopt csv-align-style 'auto)
  (call-interactively #'csv-align-fields))

(defun casual-csv-align-left ()
  "Left align CSV fields."
  (interactive)
  (setopt csv-align-style 'left)
  (call-interactively #'csv-align-fields))

(defun casual-csv-align-centre ()
  "Centre align CSV fields."
  (interactive)
  (setopt csv-align-style 'centre)
  (call-interactively #'csv-align-fields))

(defun casual-csv-align-right ()
  "Right align CSV fields."
  (interactive)
  (setopt csv-align-style 'right)
  (call-interactively #'csv-align-fields))


;; -------------------------------------------------------------------
;; Transients
(transient-define-prefix casual-csv-align-tmenu ()
  ["Align"
   :description (lambda () (format
                       "Casual CSV Align: %s %s"
                       (buffer-name)
                       (capitalize (symbol-name csv-align-style))))
   :class transient-row
   ("a" "Auto" casual-csv-align-auto :transient t)
   ("l" "Left" casual-csv-align-left :transient t)
   ("c" "Centre" casual-csv-align-centre :transient t)
   ("r" "Right" casual-csv-align-right :transient t)
   ("t" "Toggle" csv-align-mode :transient t)]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Done" casual-lib-quit-all)
          (casual-lib-quit-all)])

(provide 'casual-csv-utils)
;;; casual-csv-utils.el ends here
