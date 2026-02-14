;;; casual-org.el --- Transient UI for Org mode -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `org-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-org-tmenu' to your
;; key binding of preference.

;; (require 'casual-org) ; optional if using autoloaded menu
;; (keymap-set org-mode-map "M-m" #'casual-org-tmenu)
;; (keymap-set org-table-fedit-map "M-m" #'casual-org-table-fedit-tmenu)

;;; Code:
(require 'org)
(require 'casual-org-settings)
(require 'casual-org-utils)

;;;###autoload (autoload 'casual-org-tmenu "casual-org" nil t)
(transient-define-prefix casual-org-tmenu ()
  "Main menu for Casual Org.

Top level menu for Casual Org. The menu offering is context-dependent on
where the point is located in an Org (`org-mode') document.

Casual Org is opinionated in that it endeavors to provide an effective
set of commands based on what type of Org section the point is in. Major
sections supported by this menu include:

- Heading
- Item
- Table
- Block

This menu does not make effort to provide exhaustive coverage of all
possible Org commands.

While this menu is raised, point navigation is supported using standard
Emacs key bindings for movement."

  :refresh-suffixes t

  ;; Context-Specific
  casual-org-heading-group
  casual-org-item-group
  casual-org-table-group
  ;; casual-org-TBLFM-group
  casual-org-keyword-group
  casual-org-body-group
  casual-org-block-group

  ;; Common
  casual-org-utility-group
  casual-org-navigation-group


  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-org-settings-tmenu)
   ("I" "ⓘ" casual-org-info
    :description (lambda () (casual-org-unicode-get :info)))
   ("U" "Undo" undo :transient t)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])


;;;###autoload (autoload 'casual-org-table-fedit-tmenu "casual-org" nil t)
(transient-define-prefix casual-org-table-fedit-tmenu ()
  "Menu for Org table formula editing.

This menu provides commands for inserting formula references."
  ["Org Table Formula Edit"
   :pad-keys t
   ["Row"
    ("@<" "First Row" casual-org-table-fedit-first-row-reference)
    ("@>" "Last Row" casual-org-table-fedit-last-row-reference)]

   ["Column"
    ("$<" "First" casual-org-table-fedit-first-column-reference)
    ("$>" "Last" casual-org-table-fedit-last-column-reference)]

   ["---"
    ("1" "First (@I)" casual-org-table-fedit-first-hline-reference)
    ("2" "Second (@II)" casual-org-table-fedit-second-hline-reference)
    ("r" "Range (@I..@II)" casual-org-table-fedit-hline-range-reference)]

   [""
    ("I" "ⓘ References" casual-org-table-info-references
     :description (lambda () (format "%s References"
                                (casual-org-unicode-get :info))))]]

  casual-org-table-fedit-navigation-group
  casual-lib-navigation-group-with-undo-and-return)

(provide 'casual-org)
;;; casual-org.el ends here
