;;; casual-org-utils.el --- Casual Org Utils -*- lexical-binding: t; -*-

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
(require 'rect)
(require 'org)
(require 'casual-lib)

(defconst casual-org-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:cycle . '("⥅" "Cycle"))
    (:shift-cycle . '("⥆" "S-Cycle"))
    (:up . '("↑" "Up"))
    (:down . '("↓" "Down"))
    (:left . '("←" "Left"))
    (:right . '("→" "Right"))
    (:beginning-of-line . '("⇤" "BoL"))
    (:end-of-line . '("⇥" "EoL"))
    (:beginning-of-line-table . '("⇤" "Begin"))
    (:end-of-line-table . '("⇥" "End"))
    (:beginning-of-field . '("⇤" "Begin"))
    (:end-of-field . '("⇥" "End"))
    (:beginning-of-buffer . '("⇱" "Beginning"))
    (:end-of-buffer . '("⇲" "End"))
    (:info-functions . '("ⓘ 𝑓(𝑥)" "Info f(x)"))
    (:info . '("ⓘ" "Info"))
    (:clock-in . '("🕘 in" "In"))
    (:clock-out . '("🕔 out" "Out"))
    (:clock-report . '("🕒 🧾" "Report"))
    (:paragraph . '("¶" "Paragraph"))
    (:update . '("⟳" "Update"))
    (:kill . '("×" "Close"))
    (:see-also . '("👀" "See Also")))

  "Unicode symbol DB to use for org Transient menus.")

(defun casual-org-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-org-unicode-db))

(defun casual-org-info ()
  "Open Info for Org manual based on context.

Depending on where the point is, this command will open the point for
Org documentation on the type of section it is in. Sections supported include:

- Headlines
- Plain Lists
- Tables
- Blocks
- Drawers
- Property Syntax
- Timestamps

If none of the above sections are determined at point, then the top
level of the Org manual is opened."
  (interactive)
  (let ((node (cond
               ((org-at-heading-p) "(org) Headlines")
               ((org-at-item-p) "(org) Plain Lists")
               ((or (org-at-table-p) (org-at-TBLFM-p)) "(org) Tables")
               ((org-at-block-p) "(org) Blocks")
               ((org-at-drawer-p) "(org) Drawers")
               ((org-at-property-p) "(org) Property Syntax")
               ((org-at-timestamp-p) "(org) Timestamps")
               (t "(org) Top"))))
    (info node)))

;; TODO: not clear why this is need to get the Transient invocation to work.
(defun casual-org-deactivate-mark ()
  "Deactivate mark using function `deactivate-mark'."
  (interactive)
  (deactivate-mark))


;; -------------------------------------------------------------------
;; Org List Functions

(defun casual-org-checkbox-in-progress ()
  "If point is on an Org list item, set it to be a checkbox in-progress."
  (interactive)
  (if (org-at-item-checkbox-p)
      (org-ctrl-c-ctrl-c '(16))
    (org-ctrl-c-ctrl-c '(4))))

(defun casual-org-toggle-list-to-checkbox ()
  "If point is on an Org list item, toggle if the list item is also a checkbox.
Note that this function does not toggle the actual value of a checkbox,
which is done with `org-ctrl-c-ctrl-c'."
  (interactive)
  (org-ctrl-c-ctrl-c '(4)))


;; -------------------------------------------------------------------
;; Org Block & Table Functions

(defun casual-org-assign-name (name)
  "Insert Org NAME keyword for block or table."
  (interactive "sName: ")
  (unless (or (org-at-table-p) (org-at-block-p))
    (error "Not in a block or table"))

  (org-backward-paragraph)
  (org-end-of-line)
  (insert (format "\n#+NAME: %s" name)))



;; -------------------------------------------------------------------
;; Org Section Descriptions

(defun casual-org--block-description ()
  "Description string for an Org block."
  (let* ((context (org-element-context))
         (context-type (org-element-type context)))
    (cond
     ((eq context-type 'src-block)
      (format "Org Source (%s)"
              (org-element-property
               :language
               context)))

     ((eq context-type 'example-block)
      "Org Example Block")

     ((eq context-type 'export-block)
      (format "Org Export (%s)"
              (org-element-property
               :type
               context)))

     ((eq context-type 'center-block)
      "Org Center Block")

     ((eq context-type 'quote-block)
      "Org Quote Block")

     ((eq context-type 'verse-block)
      "Org Verse Block")

     ((eq context-type 'dynamic-block)
      "Org Dynamic Block")

     (t "Org Block: Unknown"))))

(defun casual-org--body-description ()
  "Description string for Org body."
    (cond
     ((org-at-property-drawer-p)
      "Org Property Drawer")

     ((org-at-drawer-p)
      (let ((key (org-element-property :drawer-name (org-element-context)))
            (structure-type "Drawer"))
        (if key
            (format "Org %s: %s" structure-type key)
          (format "Org %s" structure-type))))

     ((org-at-property-p)
      (let ((key (org-element-property :key (org-element-context)))
            (structure-type "Property"))
        (if key
            (format "Org %s: %s" structure-type key)
          (format "Org %s" structure-type))))

     ((org-at-clock-log-p)
      "Org Clock Log")

     ((org-in-src-block-p)
      "Org Source Body")

     (t
      (let ((heading (org-get-heading t nil t t)))
        (if heading
            (format "Org Body: %s" (substring-no-properties heading))
          (format "Org Body: %s" (buffer-name)))))))

(defun casual-org--keyword-description ()
  "Description string for Org keyword."
  ;; TODO: deal with affiliate keywords like PLOT.

  (cond
   ((org-at-TBLFM-p) "Org Table Formula (TBLFM)")
   (t (let* ((context (org-element-context))
             (key (org-element-property :key context)))
        (if key
            (format "Org Keyword: %s" key)
          "Org Keyword")))))

(defun casual-org--heading-description ()
  "Description string for Org heading."
  (let ((heading (org-get-heading t nil t t)))
    (if heading
        (format "Org Headline: %s" (substring-no-properties heading))
      (format "Org: %s" (buffer-name)))))

(defun casual-org--item-description ()
  "Description string for Org item."
  ;; TODO: Figure out how to item content.
  ;; (buffer-substring-no-properties
  ;;  (org-element-contents-begin (org-element-context))
  ;;  (org-element-contents-end (org-element-context)))
  (let ((heading (org-get-heading t nil t t)))
    (if heading
        (format "Org List: %s" (substring-no-properties heading))
      (format "Org List: %s" (buffer-name)))))


;; -------------------------------------------------------------------
;; Org Table Functions

(defun casual-org-table--cell-at-point ()
  "At point, return the cell object from an Org table.

A cell object is defined to be a list containing the row and the
column, successively."
  (if (not (org-at-table-p))
      (error "Not in a table"))

  (let* ((row (org-table-current-dline))
         (col (org-table-current-column)))
    (list row col)))

(defun casual-org-table--format-field-reference (cell)
  "Format CELL object into @r$c format.

CELL object obtained via `casual-org-table--cell-at-point'.

See Info node `(org) References' for more on Org table field
reference format."
  (let ((row (nth 0 cell))
        (col (nth 1 cell)))
    (format "@%d$%d" row col)))

(defun casual-org-table--range ()
  "Return range object from a region defined within an Org table.

A range object is a list of two cells computed via
`casual-org-table--cell-at-point', the first being the cell at the
start of the region and the last being the cell at the end of the
region."
  (if (not (and (org-at-table-p) (use-region-p)))
      (error "Not in an Org table"))

  (let* ((end-cell (casual-org-table--cell-at-point))
         (start (region-beginning))
         (end (region-end)))
      (exchange-point-and-mark)
      (let ((start-cell (casual-org-table--cell-at-point)))
        (push-mark start nil t)
        (goto-char end)
        (list start-cell end-cell))))

(defvar casual-org-table--last-reference nil
  "Last stored Org table reference.

State variable to store an Org table reference (field or range)
to be used in an Org table formula. This variable is set via
`casual-org-table--reference-dwim'

NOTE: This state variable to work-around my lack of clarity on
region and mouse menu interaction.")

(defun casual-org-table--reference-dwim ()
  "Org table reference given point or region is defined.

Return Org table reference (field or range) depending on whether
a point or region is defined in an Org table.

If the region is defined over multiple columns, then a Calc
vector matrix is returned. See Info node `(org) Formula syntax
for Calc' for more.

Calling this function will set `casual-org-table--last-reference'.

See Info node `(org) References' for more on Org table field
reference format."
  (if (not (org-at-table-p))
      (error "Not in an Org table"))

  (cond
   ((use-region-p)

    (let* ((range (casual-org-table--range))
           (start (nth 0 range))
           (end (nth 1 range))
           (msg (format "%s..%s"
                        (casual-org-table--format-field-reference start)
                        (casual-org-table--format-field-reference end))))
      (setq casual-org-table--last-reference (casual-org-table--range-to-reference range))
      msg))

   (t
    (let ((msg (casual-org-table--format-field-reference (casual-org-table--cell-at-point))))
      (setq casual-org-table--last-reference msg)
      msg))))

(defun casual-org-table-copy-reference-dwim ()
  "Copy Org table reference (field or range) into kill ring.

Given a point or region defined in an Org table, add to the
`kill-ring' an Org table field or range reference.

If the region is defined over multiple columns, then a Calc vector
matrix is returned. See Info node `(org) Formula syntax for Calc' and
Info node `(calc) Vectors and Matrices' for more.

If the buffer *Edit Formulas* is available (usually via
`org-table-edit-formulas'), the reference will be inserted into
it.

See Info node `(org) References' for more on Org table field
reference format."
  (interactive)
  (if (not (org-at-table-p))
      (error "Not in an Org table"))

  (let ((msg (casual-org-table--reference-dwim))
        (formulas-buffer (get-buffer "*Edit Formulas*")))
    (if formulas-buffer
        (with-current-buffer formulas-buffer
          (insert casual-org-table--last-reference)))
    (message "Range: %s, Copied %s" msg casual-org-table--last-reference)
    (kill-new casual-org-table--last-reference)))

(defun casual-org-table-copy-reference-and-deactivate-dwim ()
  "Copy Org table reference (field or range) into kill ring and deactivate mark.

Given a point or region defined in an Org table, add to the `kill-ring'
an Org table field or range reference.

If the region is defined over multiple columns, then a Calc vector
matrix is returned. See Info node `(org) Formula syntax for Calc' and
Info node `(calc) Vectors and Matrices' for more.

If the buffer *Edit Formulas* is available (usually via
`org-table-edit-formulas'), the reference will be inserted into it.

See Info node `(org) References' for more on Org table field
reference format."
  (interactive)
  (casual-org-table-copy-reference-dwim)
  (if mark-active
      (deactivate-mark)))

(defun casual-org-table--range-to-reference (range)
  "Convert RANGE object to Org table reference (field or range).

If the region is defined over multiple columns, then a Calc
vector matrix is returned. See Info node `(org) Formula syntax
for Calc' for more.

See `casual-org-table--range' for more on RANGE object."
  (let* ((start (nth 0 range))
         (end (nth 1 range))
         (a (nth 0 start))
         (b (nth 1 start))
         (c (nth 0 end))
         (d (nth 1 end))

         (r1 (apply #'min (list a c)))
         (c1 (apply #'min (list b d)))

         (r2 (apply #'max (list a c)))
         (c2 (apply #'max (list b d)))

         (rowrange (number-sequence r1 r2))
         (buflist (list)))


    (cond
     ((and (= r1 r2) (= c1 c2))
      (format "@%d$%d" r1 c1 ))

     ((or (= c1 c2) (= r1 r2))
      (format "@%d$%d..@%d$%d" r1 c1 r2 c2))

     (t
      (mapc (lambda (r)
              (push (format "@%d$%d..@%d$%d" r c1 r c2) buflist))
            rowrange)

      (format "vec(%s)"
              (string-join (reverse buflist) ", "))))))


(defun casual-org-table-fill-down (rows)
  "Fill table down with count of ROWS."
  (interactive "nRows: ")
  (while (> rows 0)
      ;;(message "%d" rows)
      (call-interactively #'org-table-copy-down)
      (setq rows (1- rows))))

(defun casual-org-table-kill-field-as-copy ()
  "Kill field as copy."
  (interactive)
  (let ((value (string-trim (org-table-get-field))))
    (message "Copied '%s' to kill ring." value)
    (kill-new value)))


;; -------------------------------------------------------------------
;; fedit functions

(defun casual-org-table-fedit-first-row-reference ()
  "First row reference."
  (interactive)
  (insert "@<"))

(defun casual-org-table-fedit-last-row-reference ()
  "Last row reference."
  (interactive)
  (insert "@<"))

(defun casual-org-table-fedit-first-column-reference ()
  "First column reference."
  (interactive)
  (insert "$<"))

(defun casual-org-table-fedit-last-column-reference ()
  "Last column reference."
  (interactive)
  (insert "$>"))

(defun casual-org-table-fedit-first-hline-reference ()
  "First hline reference."
  (interactive)
  (insert "@I"))

(defun casual-org-table-fedit-second-hline-reference ()
  "Second hline reference."
  (interactive)
  (insert "@II"))

(defun casual-org-table-fedit-hline-range-reference ()
  "Horizontal range reference."
  (interactive)
  (insert "@I..@II"))

(defun casual-org-table-info-references ()
  "Info for Org table references."
  (interactive) (info "(org) References" ))

(defun casual-org-table-info-calc-functions ()
  "Info for Calc functions."
  (interactive)
  (info "(calc) Function Index"))


;; -------------------------------------------------------------------
;; Transients

;; Transient Groups
(transient-define-group casual-org-table-group
  ["Org Table"
   :if org-at-table-p
   :description (lambda () (format "Org Table: %s" (casual-org-table--reference-dwim)))
   ["Table"
    :inapt-if casual-lib-buffer-read-only-p
    ("w" "Copy Field" casual-org-table-kill-field-as-copy
     :inapt-if use-region-p
     :transient t)
    ("r" "Copy Reference" casual-org-table-copy-reference-and-deactivate-dwim
     :transient t)
    ("m" "Mark Rectangle" rectangle-mark-mode :transient t)
    ;; ("{" "Toggle Debugger" org-table-toggle-formula-debugger :transient t)
    ("d" "Fill Down…" casual-org-table-fill-down :transient nil)
    ("l" "Layout›" casual-org-table-structure-tmenu )
    ("n" "Assign Name…" casual-org-assign-name)
    ("E" "Export…" org-table-export)
    ("p" "Plot" org-plot/gnuplot :transient t)]

   ["Edit"
    :pad-keys t
    ("C-SPC" "Mark" set-mark-command :transient t)
    ("SPC" "Unmark" casual-org-deactivate-mark
     :inapt-if (lambda () (if mark-active nil t))
     :transient t)
    ("`" "Field" org-table-edit-field
     :inapt-if casual-lib-buffer-read-only-p
     :transient nil)
    ("C-y" "Paste (yank)" org-yank
     :inapt-if casual-lib-buffer-read-only-p
     :transient t)
    ("=" "Formula*" org-table-eval-formula
     :inapt-if casual-lib-buffer-read-only-p
     :transient t
     :description (lambda () (if prefix-arg "Field Formula" "Column Formula✦")))
    ("DEL" "Blank" org-table-blank-field
     :inapt-if casual-lib-buffer-read-only-p
     :transient t)
    ("F" "Formulas" org-table-edit-formulas
     :inapt-if casual-lib-buffer-read-only-p
     :transient nil)]

   ;; !!! these commands alter the region selected which breaks reading a table
   ;; !!! cell. As such they can not be supported until these commands are
   ;; !!! fixed.

   ["Region"
    ("W" "Copy" org-table-copy-region :transient t)
    ("C" "Cut" org-table-cut-region
     :inapt-if casual-lib-buffer-read-only-p
     :transient t)
    ("Y" "Paste" org-table-paste-rectangle
     :inapt-if casual-lib-buffer-read-only-p
     :transient t)]

   ["Compute"
    :inapt-if casual-lib-buffer-read-only-p
    ("c" "Row" org-table-recalculate
     :description (lambda () (if prefix-arg "Table" "Row✦"))
     :transient t)
    ("g" "All" org-table-recalculate-buffer-tables :transient t)
    ("s" "Sum" org-table-sum :transient t)
    ("S" "Sort" org-table-sort-lines :transient t)
    ("T" "Transpose" org-table-transpose-table-at-point :transient t)
    ("f" "ⓘ 𝑓(𝑥)" casual-org-table-info-calc-functions
     :description (lambda () (format "%s 𝑓(𝑥)" (casual-org-unicode-get :info))))]

   ["Display"
    ("z" "Shrink Column" org-table-toggle-column-width :transient t)
    ("Z" "Shrink Table" org-table-shrink :transient t)
    ("t" "Toggle Coordinates" org-table-toggle-coordinate-overlays
     :description (lambda () (casual-lib-checkbox-label
                         org-table-overlay-coordinates
                         "@𝑟$𝑐"))
     :transient t)
    ("h" "Header Mode" org-table-header-line-mode
     :description (lambda () (casual-lib-checkbox-label
                         org-table-header-line-mode
                         "Header"))
     :transient t)
    ("V" "Line Wrap" visual-line-mode
     :description (lambda () (casual-lib-checkbox-label
                         visual-line-mode
                         "Line Wrap"))
     :transient t)]])


;; TODO: ("c" "Capture…" org-capture)
(transient-define-group casual-org-heading-group
  ["Org Heading"
   :if org-at-heading-p
   :inapt-if casual-lib-buffer-read-only-p
   :description casual-org--heading-description

   ["Headline"
    :pad-keys t
    ("t" "TODO State…" org-todo)
    ("s" "Sort…" org-sort)
    ("c" "Clone…" org-clone-subtree-with-time-shift)]

   ["Add"
    ("a" "Headline" org-insert-heading)
    ("T" "TODO" org-insert-todo-heading)]

   ["Annotate"
    ("p" "Property…" org-set-property)
    (":" "Tags…" org-set-tags-command)]

   ["Date"
    ("C-s" "Schedule…" org-schedule)
    ("C-d" "Deadline…" org-deadline)]

   ["Priority"
    :pad-keys t
    ("S-<up>" "↑" org-priority-up
     :description (lambda () (casual-org-unicode-get :up))
     :transient t)
    ("S-<down>" "↓" org-priority-down
     :description (lambda () (casual-org-unicode-get :down))
     :transient t)]

   ["Misc"
    ("n" "Note…" org-add-note)
    ("w" "Refile…" org-refile)]])


(transient-define-group casual-org-item-group
  ["Org Item"
   :if org-at-item-p
   :description casual-org--item-description

   ["Item"
    :pad-keys t
    :inapt-if casual-lib-buffer-read-only-p
    ("a" "Add" org-insert-item :transient t)
    ("c" "Cycle" org-cycle-list-bullet :transient t)
    ("b" "Toggle Checkbox" casual-org-toggle-list-to-checkbox :transient t)]

   ["Checkbox"
    :pad-keys t
    :inapt-if-not (lambda () (and (org-at-item-checkbox-p)
                             (not (casual-lib-buffer-read-only-p))))
    ("C-c" "Toggle" org-ctrl-c-ctrl-c
     :transient nil)
    ("-" "In Progress" casual-org-checkbox-in-progress :transient nil)]

   [""
    :inapt-if casual-lib-buffer-read-only-p
    ("s" "Sort…" org-sort)]])



(transient-define-group casual-org-block-group
  ["Org Block"
   :if org-at-block-p
   :inapt-if casual-lib-buffer-read-only-p
   :description casual-org--block-description
   [("'" "Edit" org-edit-src-code :transient nil)]
   [("n" "Assign Name…" casual-org-assign-name)]
   [("C-c" "Eval" org-ctrl-c-ctrl-c
     :if (lambda () (or (eq (org-element-type (org-element-context)) 'src-block)
                   (eq (org-element-type (org-element-context)) 'dynamic-block)))
     :transient t)]])


(transient-define-group casual-org-body-group
  ["Org Body"
   :if-not (lambda () (or (org-at-heading-or-item-p)
                     (org-at-table-p)
                     (org-at-block-p)
                     (org-at-keyword-p)))
   :description casual-org--body-description
   :inapt-if casual-lib-buffer-read-only-p

   ;; !!!: Body
   ["To"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-drawer-p) ; covers property-drawer-p
                      (org-at-clock-log-p)
                      (org-in-src-block-p)
                      (org-at-property-p)))
    ("*" "Heading" org-ctrl-c-star :transient t)
    ("-" "Item" org-ctrl-c-minus :transient t)]

   ["Add"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-drawer-p) ; covers property-drawer-p
                      (org-at-clock-log-p)
                      (org-in-src-block-p)
                      (org-at-property-p)))
    ("b" "Block…" org-insert-structure-template)
    ("d" "Drawer…" org-insert-drawer)]


   ;; !!!: org-in-src-block-p
   [:if org-in-src-block-p
    ("'" "Edit" org-edit-src-code :transient nil)]

   [:if org-in-src-block-p
    ("C-c" "Eval" org-ctrl-c-ctrl-c
     :if (lambda () (or (eq (org-element-type (org-element-context)) 'src-block)
                   (eq (org-element-type (org-element-context)) 'dynamic-block)))
     :transient t)]

   ;; !!!: org-at-property-drawer-p
   [:if org-at-property-drawer-p
    ("p" "Add Property…" org-set-property)]

   ;; !!!: org-at-property-p
   [:if org-at-property-p
    ("p" "Add Property…" org-set-property)]

   [:if org-at-property-p
    ("a" "Action…" org-property-action)]

   ;; !!!: org-at-drawer-p
   [:if (lambda () (and (org-at-drawer-p)
                   (not (org-at-property-drawer-p))))
    ("TAB" "Cycle…" org-cycle :transient t)]

   ;; !!!: org-at-clock-log-p
   ["Clock"
    :pad-keys t
    :if org-at-clock-log-p
    ("M-c" "🕘 in" org-clock-in
     :description (lambda () (casual-org-unicode-get :clock-in))
     :if-not org-clocking-p)
    ("M-c" "🕔 out" org-clock-out
     :description (lambda () (casual-org-unicode-get :clock-out))
     :if org-clocking-p)]

   ["Timestamp"
    :if org-at-clock-log-p
    ("u" "Adjust Up" org-clock-timestamps-up
     :description (lambda () (format "Adjust %s" (casual-org-unicode-get :up)))
     :transient t)
    ("d" "Adjust Down" org-clock-timestamps-down
     :description (lambda () (format "Adjust %s" (casual-org-unicode-get :down)))
     :transient t)]])


(transient-define-group casual-org-keyword-group
  ["Org Keyword"
   :if org-at-keyword-p
   :description casual-org--keyword-description
   :inapt-if casual-lib-buffer-read-only-p
   [:if org-at-TBLFM-p
    ("F" "Edit Formulas" org-table-edit-formulas :transient nil)]

   [:if org-at-TBLFM-p
    ("C-c" "Eval" org-table-recalculate-buffer-tables :transient nil)]

   ;; TODO: Does this apply to all affiliate keywords?
   [:if-not org-at-TBLFM-p
    :inapt-if (lambda () (org-element-property :key (org-element-context)))
    ("C-c" "Eval" org-ctrl-c-ctrl-c :transient nil)]])


(transient-define-group casual-org-navigation-group
  [
   ["Field"
    :if org-at-table-p
    ("M-a" "⇤" org-table-beginning-of-field
     :description (lambda () (casual-org-unicode-get :beginning-of-field))
     :transient t)]

   [""
    :if org-at-table-p
    ("M-e" "⇥" org-table-end-of-field
     :description (lambda () (casual-org-unicode-get :end-of-field))
     :transient t)]

   ["Mark"
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-table-p)
                      (org-at-block-p)))
    ("ms" "Subtree" org-mark-subtree)]

   [""
    :if-not (lambda () (or (org-at-keyword-p)
                      (org-at-table-p)
                      (org-at-block-p)))
    ("me" "Element" org-mark-element)]

   ["Util"
    ("v" "Copy Visible"
     org-copy-visible
     :inapt-if-not (lambda () (use-region-p)))]
   [""
    ("e" "Export…" org-export-dispatch)]])



(transient-define-group casual-org-utility-group
  [:if-not (lambda () (or (org-at-table-p)
                     (org-at-TBLFM-p)
                     (org-at-block-p)
                     (org-at-property-p)
                     (org-at-drawer-p)
                     (org-at-clock-log-p)
                     (org-in-src-block-p)
                     (org-at-keyword-p)))

   ["Link"
    :inapt-if casual-lib-buffer-read-only-p
    ("l" "Insert…" org-insert-link)
    ("L" "Last" org-insert-last-stored-link)
    ("r" "Cite…" org-cite-insert)]

   ["Timestamp"
    :inapt-if casual-lib-buffer-read-only-p
    ("." "Add…" org-timestamp)
    ("i" "Inactive…" org-timestamp-inactive)]

   ["Clock"
    :pad-keys t
    :inapt-if casual-lib-buffer-read-only-p
    ("M-c" "🕘 in" org-clock-in
     :description (lambda () (casual-org-unicode-get :clock-in))
     :if-not org-clocking-p)
    ("M-c" "🕔 out" org-clock-out
     :description (lambda () (casual-org-unicode-get :clock-out))
     :if org-clocking-p)
    ("R" "🕒 🧾" org-clock-report
     :description (lambda () (casual-org-unicode-get :clock-report))
     :if (lambda () (not (org-at-heading-or-item-p))))]

   ["Display"
    ("M-i" "Toggle Images" org-toggle-inline-images
     :transient nil)
    ("M" "Show Markup" visible-mode
     :description (lambda () (casual-lib-checkbox-label visible-mode "Show Markup"))
     :transient nil)
    ("P" "Toggle Prettify" prettify-symbols-mode
     :description (lambda () (casual-lib-checkbox-label prettify-symbols-mode
                                                   "Prettify"))
     :transient nil)]

   [""
    ("V" "Line Wrap" visual-line-mode
     :description (lambda () (casual-lib-checkbox-label visual-line-mode
                                                   "Line Wrap"))
     :transient t)
    ("N" "Number" org-num-mode
     :if org-at-heading-p
     :description (lambda () (casual-lib-checkbox-label org-num-mode
                                                   "Heading #"))
     :transient t)]])


(transient-define-prefix casual-org-table-structure-tmenu ()
  "Menu for Org Table structure (layout) commands."
  :refresh-suffixes t
  :transient-non-suffix t

  ["Org Table Layout"
   :pad-keys t
   :inapt-if-not org-at-table-p
   ["Insert"
    :inapt-if casual-lib-buffer-read-only-p
    ("r" "Row" org-table-insert-row :transient t)
    ("c" "Column" org-table-insert-column :transient t)
    ("-" "H Line" org-table-insert-hline :transient t)]

   ["Delete"
    :inapt-if casual-lib-buffer-read-only-p
    ("DEL" "Row" org-table-kill-row :transient t)
    ("M-DEL" "Column" org-table-delete-column :transient t)]

   ["Move"
    :inapt-if casual-lib-buffer-read-only-p
    ("M-p" "Row ↑" org-table-move-row-up :transient t)
    ("M-n" "Row ↓" org-table-move-row-down :transient t)]

   [""
    :inapt-if casual-lib-buffer-read-only-p
    ("M-b" "Column ←" org-table-move-column-left :transient t)
    ("M-f" "Column →" org-table-move-column-right :transient t)]]

  ;; TODO: Support <r> <c> <l> (org) Column Width and Alignment

  ["Field"
   [("M-a" "⇤" org-table-beginning-of-field :transient t)]
   [("M-e" "⇥" org-table-end-of-field :transient t)]]

  casual-lib-navigation-group-with-undo-and-return)

(provide 'casual-org-utils)
;;; casual-org-utils.el ends here
