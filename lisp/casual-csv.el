;;; casual-csv.el --- Transient UI for CSV mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Charles Y. Choi

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

;; Casual CSV is a Transient user interface for the CSV library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual CSV by running the hook
;; function `casual-csv-init'.

;; Ensure that `casual-csv-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) CSV Install' for more detail on
;; installation.

;;; Code:
(require 'casual-editkit-utils)
(require 'casual-csv-settings)
(require 'casual-csv-utils)

;;;###autoload (autoload 'casual-csv-init "casual-csv" nil t)
(defun casual-csv-init ()
  "Initialize and configure Casual CSV.

This hook binds `casual-csv-tmenu' to `casual-keybinding-secondary'.

If `casual-csv-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-csv-setup' will be set."
  (add-hook 'csv-mode-hook #'casual-csv-setup))

(defun casual-csv-setup ()
  "Setup `csv-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set csv-mode-map casual-keybinding-secondary #'casual-csv-tmenu))


;;;###autoload (autoload 'casual-csv-tmenu "casual-csv" nil t)
(transient-define-prefix casual-csv-tmenu ()
  :refresh-suffixes t
  ["Casual CSV"
   :description (lambda () (format
                       "Casual CSV: %s [%d,%d] %s"
                       (buffer-name)
                       (line-number-at-pos)
                       (csv--field-index)
                       (capitalize (symbol-name csv-align-style))))
   :pad-keys t

   ["Navigation"
    ("S-TAB" "←" csv-backtab-command
     :description (lambda () (format "%s" (casual-csv-unicode-get :left)))
     :transient t)
    ("TAB" "→" csv-tab-command
     :description (lambda () (format "%s" (casual-csv-unicode-get :right)))
     :transient t)]
   [""
    ("p" "↑" previous-line
     :description (lambda () (format "%s" (casual-csv-unicode-get :up)))
     :transient t)
    ("n" "↓" next-line
     :description (lambda () (format "%s" (casual-csv-unicode-get :down)))
     :transient t)]
   ["Line"
    ("C-a" "⇤" move-beginning-of-line
     :description (lambda () (format "%s" (casual-csv-unicode-get :bol)))
     :transient t)
    ("C-e" "⇥" move-end-of-line
     :description (lambda () (format "%s" (casual-csv-unicode-get :eol)))
     :transient t)]
   ["Buffer"
    ("<" "⇱" beginning-of-buffer
     :description (lambda () (format "%s" (casual-csv-unicode-get :beginning-of-buffer)))
     :transient t)
    (">" "⇲" end-of-buffer
     :description (lambda () (format "%s" (casual-csv-unicode-get :end-of-buffer)))
     :transient t)]

   ["Page"
    ("M-v" "Up" scroll-down-command :transient t)
    ("C-v" "Down" scroll-up-command :transient t)]

   ["Buffer/File"
    ("a" "Align›" casual-csv-align-tmenu)
    ("v" "View" view-mode
     :if (lambda () (not buffer-read-only))
     :transient t)
    ("e" "Edit" View-exit
     :if (lambda () buffer-read-only)
     :transient t)
    ("d" "Duplicate" casual-lib-duplicate-file)]]

  [["Field"
    :pad-keys t
    ("m" "Mark" mark-sexp)
    ("c" "Copy" casual-editkit-copy-sexp)]

   ["Sort"
    :if (lambda () (not buffer-read-only))
    ("s" "Fields" csv-sort-fields)
    ("N" "Numeric" csv-sort-numeric-fields)
    ("r" "Reverse" csv-reverse-region)]

   ["Fields"
    :if (lambda () (not buffer-read-only))
    ("k" "Kill∙" csv-kill-fields)
    ("y" "Yank∙" csv-yank-fields)]

   ["Misc"
    ("t" "Transpose" csv-transpose
     :if (lambda () (not buffer-read-only)))
    ("S" "Separator…" csv-set-separator)
    ("o" "Occur…" occur)
    ("C" "Copy as Table" casual-csv-kill-region-as-org-table
     :inapt-if-not use-region-p)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-csv-settings-tmenu)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)
   ("q" "Quit" quit-window)])

(provide 'casual-csv)
;;; casual-csv.el ends here
