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

;; Casual Org is a Transient user interface for the Org library.

;; INSTALLATION

;; By default, `casual-init' will setup Casual Org by running the hook
;; function `casual-org-init'.

;; Ensure that `casual-org-init' is included in the customizable hook
;; variable `casual-init-hook'.

;; Consult the Info node `(casual) Org Install' for more detail on
;; installation.

;;; Code:
(require 'org)
(require 'casual-org-settings)
(require 'casual-org-utils)

;;;###autoload (autoload 'casual-org-init "casual-org" nil t)
(defun casual-org-init ()
  "Initialize and configure Casual Org.

This hook binds `casual-org-tmenu' to `casual-keybinding-secondary'.

If `casual-org-add-extra-keybindings' is non-nil, then extra
keybindings specified in `casual-org-setup' will be set."
  (add-hook 'org-mode-hook #'casual-org-setup))

(defun casual-org-setup ()
  "Setup `org-mode' for Casual.

To see what keybindings are set by this function, press ‘s’ to view its
source."
  (keymap-set org-mode-map casual-keybinding-secondary #'casual-org-tmenu)
  (keymap-set org-table-fedit-map casual-keybinding-secondary #'casual-org-table-fedit-tmenu))

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
  :transient-non-suffix t

  ;; Context-Specific
  casual-org-heading-group
  casual-org-item-group
  casual-org-table-group
  casual-org-keyword-group
  casual-org-body-group
  casual-org-block-group

  ;; Common
  casual-org-utility-group
  casual-org-navigation-group

  [:class transient-row
   :if casual-org-mode-p
   (casual-lib-quit-one)
   ("," "Settings›" casual-org-settings-tmenu)
   ("I" "ⓘ" casual-org-info
    :description (lambda () (casual-org-unicode-get :info)))
   ("U" "Undo" undo :transient t)
   ("RET" "Done" transient-quit-all)
   (casual-lib-quit-all)])


;;;###autoload (autoload 'casual-org-table-fedit-tmenu "casual-org" nil t)
(transient-define-prefix casual-org-table-fedit-tmenu ()
  "Menu for Org table formula editing.

This menu provides commands for inserting formula references."
  :transient-non-suffix t

  ["Org Table Formula Edit"
   :pad-keys t
   ["Row"
    ("@<" "⤒ First" casual-org-table-fedit-first-row-reference
     :description (lambda () (casual-org-unicode-get :first-row)))
    ("@>" "⤓ Last" casual-org-table-fedit-last-row-reference
     :description (lambda () (casual-org-unicode-get :last-row)))]

   ["Column"
    ("$<" "⇤ First" casual-org-table-fedit-first-column-reference
     :description (lambda () (casual-org-unicode-get :first-column)))
    ("$>" "⇥ Last" casual-org-table-fedit-last-column-reference
     :description (lambda () (casual-org-unicode-get :last-column)))]

   ["H Line (-)"
    ("1" "First (@I)" casual-org-table-fedit-first-hline-reference)
    ("2" "Second (@II)" casual-org-table-fedit-second-hline-reference)
    ("r" "Range (@I..@II)" casual-org-table-fedit-hline-range-reference)]

   ["Vector"
    ("s" "sum" casual-org-table-insert-calc-sum)
    ("m" "mean" casual-org-table-insert-calc-mean)
    ("a" "max" casual-org-table-insert-calc-max)
    ("z" "min" casual-org-table-insert-calc-min)]

   ["Info"
    :description (lambda () (casual-org-unicode-get :info))
    ("F" "Formula Syntax" casual-org-table-info-formula-syntax)
    ("R" "References" casual-org-table-info-references)
    ("f" "𝑓(𝑥)" casual-org-table-info-calc-functions)]]

  casual-lib-navigation-group-with-undo-and-return)

(provide 'casual-org)
;;; casual-org.el ends here
