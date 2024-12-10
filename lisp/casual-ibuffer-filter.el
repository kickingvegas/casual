;;; casual-ibuffer-filter.el --- Casual IBuffer Filter -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Charles Choi

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

(require 'transient)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'casual-lib)
(require 'casual-ibuffer-settings)
(require 'casual-ibuffer-utils)

;;; Code:
(defvar casual-ibuffer--current-collection-name nil
  "Internal variable for current filter group collection name.")

(defun casual-ibuffer-switch-to-saved-filter-groups (name)
  "Set this buffer's filter groups to saved version with NAME.
The value from `ibuffer-saved-filter-groups' is used."
  (interactive
   (list
    (cond ((null ibuffer-saved-filter-groups)
           (error "No saved filters"))
          ;; `ibuffer-saved-filter-groups' is a user variable that defaults
          ;; to nil.  We assume that with one element in this list the user
          ;; knows what she wants.  See bug#12331.
          ((null (cdr ibuffer-saved-filter-groups))
           (caar ibuffer-saved-filter-groups))
          (t
           (completing-read "Switch to saved filter group: "
                            ibuffer-saved-filter-groups nil t)))))

  (setq casual-ibuffer--current-collection-name name)
  (setq ibuffer-filter-groups (cdr (assoc name ibuffer-saved-filter-groups))
	ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil t))

(defun casual-ibuffer--ibuffer-update (arg &optional silent)
  "Advising function after `ibuffer-update' is called with ARG and SILENT."
  (ignore arg)
  (ignore silent)
  (if (not ibuffer-filter-groups)
      (setq casual-ibuffer--current-collection-name nil)))

(advice-add #'ibuffer-update :after #'casual-ibuffer--ibuffer-update)

;; Transients
;;;###autoload (autoload 'casual-ibuffer-filter-tmenu "casual-ibuffer-filter" nil t)
(transient-define-prefix casual-ibuffer-filter-tmenu ()
  "Casual IBuffer filter menu."
  :refresh-suffixes t
  ["IBuffer: Filter"
   ["Add"
    :pad-keys t
    ("SPC" "Rule…" ibuffer-filter-chosen-by-completion
     :transient t)
    ("r" "Switch to…" ibuffer-switch-to-saved-filters
     :transient t)
    ("a" "Add from saved…" ibuffer-add-saved-filters :transient t)
    ("g" "Create Filter Group…" ibuffer-filters-to-filter-group
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)]

   ["Compose"
    ("&" "and" ibuffer-and-filter
     :inapt-if (lambda () (< (length ibuffer-filtering-qualifiers) 2))
     :transient t)
    ("|" "or" ibuffer-or-filter
     :inapt-if (lambda () (< (length ibuffer-filtering-qualifiers) 2))
     :transient t)
    ("!" "not" ibuffer-negate-filter
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)
    ("t" "Exchange" ibuffer-exchange-filters
     :inapt-if (lambda () (< (length ibuffer-filtering-qualifiers) 2))
     :transient t)
    ("d" "Decompose" ibuffer-decompose-filter
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)]

   ["Remove"
    ("-" "Pop" ibuffer-pop-filter
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)
    ("/" "Clear" ibuffer-filter-disable
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)]

   ["Persist"
    ("s" "Save…" ibuffer-save-filters
     :inapt-if (lambda () (null ibuffer-filtering-qualifiers))
     :transient t)
    ("x" "Delete…" ibuffer-delete-saved-filters :transient t)]

   ["Navigation"
    :pad-keys t
    ("p" "Previous" ibuffer-backward-line
     :description (lambda ()
                    (format "%s" (casual-ibuffer-unicode-get :previous)))
     :transient t)
    ("n" "Next" ibuffer-forward-line
     :description (lambda () (format "%s" (casual-ibuffer-unicode-get :next)))
     :transient t)
    ("[" "Previous Group" ibuffer-backward-filter-group
     :description (lambda ()
                    (format "%s %s"
                            (casual-ibuffer-unicode-get :previous)
                            (casual-ibuffer-unicode-get :group)))
     :transient t)
    ("]" "Next Group" ibuffer-forward-filter-group
     :description (lambda ()
                    (format "%s %s"
                            (casual-ibuffer-unicode-get :next)
                            (casual-ibuffer-unicode-get :group)))
     :transient t)

    ("j" "Jump…" ibuffer-jump-to-buffer
     :description (lambda () (format "%s…" (casual-ibuffer-unicode-get :jump)))
     :transient t)
    ("M-j" "Jump…" ibuffer-jump-to-filter-group
     :description (lambda () (format "%s…"
                                     (casual-ibuffer-unicode-get :jumpgroup)))
     :transient t)]]

  ["Filter Group"
   :description (lambda () (format "Filter Group %s"
                                   (casual-ibuffer-unicode-get :group)))
   ["Collection"
    :description (lambda ()
                   (format "Collection%s"
                           (if casual-ibuffer--current-collection-name
                               (format
                                " (%s)"
                                casual-ibuffer--current-collection-name)
                             "")))
    ("R" "Switch to…" casual-ibuffer-switch-to-saved-filter-groups :transient t)
    ("S" "Save…" ibuffer-save-filter-groups
     :inapt-if (lambda () (not ibuffer-filter-groups))
     :transient t)
    ("X" "Delete…" ibuffer-delete-saved-filter-groups :transient t)]
   ["Modify"
    ("D" "Decompose…" ibuffer-decompose-filter-group :transient t)]
   ["Remove"
    ("P" "Pop" ibuffer-pop-filter-group :transient t)
    ("\\" "Clear all" ibuffer-clear-filter-groups :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("," "Settings›" casual-ibuffer-settings-tmenu)
          ("RET" "Visit/Toggle" casual-ibuffer-return-dwim)
          ("$" "Toggle Group" ibuffer-toggle-filter-group
           :description (lambda () (format "Toggle %s" (casual-ibuffer-unicode-get :group)))
           :inapt-if-not (lambda () (casual-ibuffer-filter-group-p))
           :transient t)
          (casual-lib-quit-all)])

(provide 'casual-ibuffer-filter)
;;; casual-ibuffer-filter.el ends here
