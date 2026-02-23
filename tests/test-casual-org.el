;;; test-casual-org.el --- Casual Make Tests -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'casual-org-test-utils)
(require 'casual-lib-test-utils)
(require 'casual-org)

(ert-deftest test-casual-org-tmenu-heading ()
  (casualt-org-setup)
  (search-forward "* Casual Org Test File" nil t)

  (cl-letf ((casualt-mock #'org-todo)
            (casualt-mock #'org-sort)
            (casualt-mock #'org-clone-subtree-with-time-shift)
            (casualt-mock #'org-insert-heading)
            (casualt-mock #'org-insert-todo-heading)
            (casualt-mock #'org-set-property)
            (casualt-mock #'org-set-tags-command)
            (casualt-mock #'org-schedule)
            (casualt-mock #'org-deadline)
            (casualt-mock #'org-priority-up)
            (casualt-mock #'org-priority-down)
            (casualt-mock #'org-add-note)
            (casualt-mock #'org-refile)
            (casualt-mock #'casual-org-settings-tmenu)
            (casualt-mock #'casual-org-info)
            (casualt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "t" :command org-todo)
             (:binding "s" :command org-sort)
             (:binding "c0" :command org-clone-subtree-with-time-shift)
             (:binding "a" :command org-insert-heading)
             (:binding "T" :command org-insert-todo-heading)
             (:binding "p" :command org-set-property)
             (:binding ":" :command org-set-tags-command)
             (:binding "C-s" :command org-schedule)
             (:binding "C-d" :command org-deadline)
             (:binding "S-<up>" :command org-priority-up)
             (:binding "S-<down>" :command org-priority-down)
             (:binding "n" :command org-add-note)
             (:binding "w" :command org-refile)

             (:binding "," :command casual-org-settings-tmenu)
             (:binding "I" :command casual-org-info)
             (:binding "RET" :command transient-quit-all)
             )))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-item ()
  (casualt-org-setup)
  (search-forward "- a" nil t)

  (cl-letf ((casualt-mock #'org-insert-item)
            (casualt-mock #'org-cycle-list-bullet)
            (casualt-mock #'casual-org-toggle-list-to-checkbox)
            (casualt-mock #'casual-org-settings-tmenu)
            (casualt-mock #'casual-org-info)
            (casualt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "a" :command org-insert-item)
             (:binding "b" :command casual-org-toggle-list-to-checkbox)
             (:binding "c" :command org-cycle-list-bullet)
             (:binding "," :command casual-org-settings-tmenu)
             (:binding "I" :command casual-org-info)
             (:binding "RET" :command transient-quit-all))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-item-checkbox ()
  (casualt-org-setup)
  (search-forward "- [ ] task 1" nil t)

  (cl-letf ((casualt-mock #'org-ctrl-c-ctrl-c)
            (casualt-mock #'casual-org-checkbox-in-progress)
            (casualt-mock #'casual-org-insert-checkbox)
            (casualt-mock #'org-cycle-list-bullet)
            (casualt-mock #'casual-org-toggle-list-to-checkbox))

    (let ((test-vectors
           '((:binding "a" :command casual-org-insert-checkbox)
             (:binding "b" :command casual-org-toggle-list-to-checkbox)
             (:binding "c" :command org-cycle-list-bullet)
             (:binding "C-c" :command org-ctrl-c-ctrl-c)
             (:binding "-" :command casual-org-checkbox-in-progress))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-TBLFM ()
  (casualt-org-setup)
  (search-forward "#+TBLFM" nil t)

  (cl-letf ((casualt-mock #'org-table-recalculate-buffer-tables)
            (casualt-mock #'org-table-edit-formulas))

    (let ((test-vectors
           '((:binding "C-c" :command org-table-recalculate-buffer-tables)
             (:binding "F" :command org-table-edit-formulas))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-body ()
  (casualt-org-setup)
  (search-forward "This is" nil t)

  (cl-letf ((casualt-mock #'org-ctrl-c-star)
            (casualt-mock #'org-ctrl-c-minus)
            (casualt-mock #'org-insert-structure-template)
            (casualt-mock #'org-insert-drawer)
            (casualt-mock #'casual-org-insert-keyword)
            )

    (let ((test-vectors
           '((:binding "*" :command org-ctrl-c-star)
             (:binding "-" :command org-ctrl-c-minus)
             (:binding "bc" :command org-insert-structure-template)
             (:binding "d" :command org-insert-drawer)
             (:binding "k" :command casual-org-insert-keyword))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-body-keyword ()
  (casualt-org-setup)
  (search-forward "#+PLOT" nil t)

  (cl-letf ((casualt-mock #'org-ctrl-c-ctrl-c))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-body-in-src-block ()
  (casualt-org-setup)
  (search-forward "(message" nil t)

  (cl-letf ((casualt-mock #'org-ctrl-c-ctrl-c))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-body-at-property-drawer ()
  (casualt-org-setup)
  (search-forward ":PROPERTIES:" nil t)

  (cl-letf ((casualt-mock #'org-set-property))
    (let ((test-vectors
           '((:binding "p" :command org-set-property))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-body-at-property ()
  (casualt-org-setup)
  (search-forward ":CREATED:" nil t)

  (cl-letf ((casualt-mock #'org-set-property)
            (casualt-mock #'org-property-action))
    (let ((test-vectors
           '((:binding "p" :command org-set-property)
             (:binding "a" :command org-property-action))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-body-at-drawer ()
  (casualt-org-setup)
  (search-forward ":LOGBOOK:" nil t)

  (cl-letf ((casualt-mock #'org-cycle))
    (let ((test-vectors
           '((:binding "TAB" :command org-cycle))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-body-at-clock-log ()
  (casualt-org-setup)
  (search-forward "CLOCK:" nil t)

  (cl-letf ((casualt-mock #'org-clock-in)
            (casualt-mock #'org-clock-timestamps-up)
            (casualt-mock #'org-clock-timestamps-down))
    (let ((test-vectors
           '((:binding "M-c" :command org-clock-in)
             (:binding "u" :command org-clock-timestamps-up)
             (:binding "d" :command org-clock-timestamps-down))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))
(ert-deftest test-casual-org-tmenu-block ()
  (casualt-org-setup)
  (search-forward "#+BEGIN_SRC" nil t)

  (cl-letf ((casualt-mock #'org-ctrl-c-ctrl-c)
            (casualt-mock #'org-edit-src-code)
            (casualt-mock #'casual-org-assign-name))
    (let ((test-vectors
           '((:binding "C-c" :command org-ctrl-c-ctrl-c)
             (:binding "'" :command org-edit-src-code)
             (:binding "n" :command casual-org-assign-name))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-tmenu-utility ()
  (casualt-org-setup)
  (search-forward "* Casual Org Test File" nil t)

  (cl-letf (((symbol-function #'display-graphic-p) (lambda (&optional display) t))
            (casualt-mock #'org-insert-link)
            (casualt-mock #'org-insert-last-stored-link)
            (casualt-mock #'org-cite-insert)
            (casualt-mock #'org-timestamp)
            (casualt-mock #'org-timestamp-inactive)
            (casualt-mock #'org-clock-in)
            (casualt-mock #'org-clock-out)
            (casualt-mock #'org-clock-report)
            (casualt-mock #'org-link-preview)
            (casualt-mock #'casual-org-toggle-images)
            (casualt-mock #'visible-mode)
            (casualt-mock #'prettify-symbols-mode)
            (casualt-mock #'visual-line-mode)
            (casualt-mock #'org-num-mode))
    (let ((test-vectors
           '((:binding "l" :command org-insert-link)
             (:binding "L" :command org-insert-last-stored-link)
             (:binding "r" :command org-cite-insert)
             (:binding "." :command org-timestamp)
             (:binding "i" :command org-timestamp-inactive)
             (:binding "M-c" :command org-clock-in)
             ;; (:binding "M-c" :command org-clock-out) ; TODO test
             ;; (:binding "R" :command org-clock-report) ; TODO test
             (:binding "M-i" :command casual-org-toggle-images)
             (:binding "M-l" :command org-link-preview)
             (:binding "M" :command visible-mode)
             (:binding "P" :command prettify-symbols-mode)
             (:binding "V" :command visual-line-mode)
             (:binding "N" :command org-num-mode))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))


  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-navigation ()
  (casualt-org-setup)
  (search-forward "* Casual Org Test File" nil t)

  (cl-letf ((casualt-mock #'org-shifttab)
            (casualt-mock #'previous-line)
            (casualt-mock #'next-line)
            (casualt-mock #'backward-char)
            (casualt-mock #'forward-char)
            (casualt-mock #'org-beginning-of-line)
            (casualt-mock #'org-end-of-line)
            (casualt-mock #'org-mark-subtree)
            (casualt-mock #'org-mark-element)
            ;; (casualt-mock #'org-copy-visible)
            (casualt-mock #'org-export-dispatch))

    (let ((test-vectors
           '((:binding "TAB" :command org-cycle)
             (:binding "S-TAB" :command org-shifttab)
             (:binding "C-p" :command previous-line)
             (:binding "C-n" :command next-line)
             (:binding "C-b" :command backward-char)
             (:binding "C-f" :command forward-char)
             (:binding "C-a" :command org-beginning-of-line)
             (:binding "C-e" :command org-end-of-line)
             (:binding "ms" :command org-mark-subtree)
             (:binding "me" :command org-mark-element)
             ;; (:binding "v" :command org-copy-visible)
             (:binding "e" :command org-export-dispatch))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-navigation-table ()
  (casualt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((casualt-mock #'org-table-beginning-of-field)
            (casualt-mock #'org-table-end-of-field))

    (let ((test-vectors
           '((:binding "M-a" :command org-table-beginning-of-field)
             (:binding "M-e" :command org-table-end-of-field))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))

(ert-deftest test-casual-org-tmenu-table ()
  (casualt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((casualt-mock #'casual-org-table-kill-field-as-copy)
            (casualt-mock #'casual-org-table-copy-reference-and-deactivate-dwim)
            (casualt-mock #'rectangle-mark-mode)
            (casualt-mock #'casual-org-table-fill-down)
            (casualt-mock #'casual-org-table-structure-tmenu)
            (casualt-mock #'casual-org-assign-name)
            (casualt-mock #'org-table-export)
            (casualt-mock #'org-plot/gnuplot)
            (casualt-mock #'set-mark-command)
            (casualt-mock #'org-table-edit-field)
            (casualt-mock #'org-yank)
            (casualt-mock #'org-table-eval-formula)
            (casualt-mock #'org-table-blank-field)
            (casualt-mock #'org-table-edit-formulas)

            (casualt-mock #'org-table-copy-region)
            (casualt-mock #'org-table-cut-region)
            (casualt-mock #'org-table-paste-rectangle)
            (casualt-mock #'org-table-recalculate)
            (casualt-mock #'org-table-recalculate-buffer-tables)
            (casualt-mock #'org-table-sum)
            (casualt-mock #'org-table-sort-lines)
            (casualt-mock #'org-table-transpose-table-at-point)
            (casualt-mock #'casual-org-table-info-calc-functions)
            (casualt-mock #'org-table-toggle-column-width)
            (casualt-mock #'org-table-shrink)
            (casualt-mock #'org-table-toggle-coordinate-overlays)
            (casualt-mock #'org-table-header-line-mode)
            (casualt-mock #'visual-line-mode))

    (let ((test-vectors
           '((:binding "w" :command casual-org-table-kill-field-as-copy)
             (:binding "r" :command casual-org-table-copy-reference-and-deactivate-dwim)
             (:binding "m" :command rectangle-mark-mode)
             (:binding "d1" :command casual-org-table-fill-down)
             (:binding "l" :command casual-org-table-structure-tmenu )
             (:binding "n" :command casual-org-assign-name)
             (:binding "E" :command org-table-export)
             (:binding "p" :command org-plot/gnuplot)

             (:binding "C-SPC" :command set-mark-command)

             ;; TODO: handle unmark
             ;; ("SPC" "Unmark" (lambda () (interactive) (deactivate-mark)))
             (:binding "`" :command org-table-edit-field)
             (:binding "C-y" :command org-yank)
             (:binding "=" :command org-table-eval-formula)
             (:binding "DEL" :command org-table-blank-field)
             (:binding "F" :command org-table-edit-formulas)

             (:binding "W" :command org-table-copy-region)
             (:binding "C" :command org-table-cut-region)
             (:binding "Y" :command org-table-paste-rectangle)

             (:binding "c" :command org-table-recalculate)
             (:binding "g" :command org-table-recalculate-buffer-tables)
             (:binding "s" :command org-table-sum)
             (:binding "S" :command org-table-sort-lines)
             (:binding "T" :command org-table-transpose-table-at-point)
             (:binding "f" :command casual-org-table-info-calc-functions)

             (:binding "z" :command org-table-toggle-column-width)
             (:binding "Z" :command org-table-shrink)
             (:binding "t" :command org-table-toggle-coordinate-overlays)
             (:binding "h" :command org-table-header-line-mode)
             (:binding "V" :command visual-line-mode))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-table-structure-tmenu ()
  (casualt-org-setup)
  (search-forward "|" nil t)
  (push-mark)
  (forward-char 2)

  (cl-letf ((casualt-mock #'org-table-beginning-of-field)
            (casualt-mock #'org-table-end-of-field)
            (casualt-mock #'org-table-insert-row)
            (casualt-mock #'org-table-insert-column)
            (casualt-mock #'org-table-insert-hline)
            (casualt-mock #'org-table-kill-row)
            (casualt-mock #'org-table-delete-column)
            (casualt-mock #'org-table-move-row-up)
            (casualt-mock #'org-table-move-row-down)
            (casualt-mock #'org-table-move-column-left)
            (casualt-mock #'org-table-move-column-right)
            (casualt-mock #'casual-org-table-insert-align-left)
            (casualt-mock #'casual-org-table-insert-align-center)
            (casualt-mock #'casual-org-table-insert-align-right)
            (casualt-mock #'casual-org-table-info-width-alignment))

    (let ((test-vectors
           '((:binding "M-a" :command org-table-beginning-of-field)
             (:binding "M-e" :command org-table-end-of-field)
             (:binding "r" :command org-table-insert-row)
             (:binding "c" :command org-table-insert-column)
             (:binding "-" :command org-table-insert-hline)
             (:binding "DEL" :command org-table-kill-row)
             (:binding "M-DEL" :command org-table-delete-column)
             (:binding "M-p" :command org-table-move-row-up)
             (:binding "M-n" :command org-table-move-row-down)
             (:binding "M-b" :command org-table-move-column-left)
             (:binding "M-f" :command org-table-move-column-right)
             (:binding "al" :command casual-org-table-insert-align-left)
             (:binding "ac" :command casual-org-table-insert-align-center)
             (:binding "ar" :command casual-org-table-insert-align-right)
             (:binding "I" :command casual-org-table-info-width-alignment))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-table-structure-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(ert-deftest test-casual-org-table-fedit-tmenu ()
  (casualt-org-setup)
  (cl-letf ((casualt-mock #'casual-org-table-fedit-first-row-reference)
            (casualt-mock #'casual-org-table-fedit-last-row-reference)
            (casualt-mock #'casual-org-table-fedit-first-column-reference)
            (casualt-mock #'casual-org-table-fedit-last-column-reference)
            (casualt-mock #'casual-org-table-fedit-first-hline-reference)
            (casualt-mock #'casual-org-table-fedit-second-hline-reference)
            (casualt-mock #'casual-org-table-fedit-hline-range-reference)
            (casualt-mock #'casual-org-table-info-references)
            (casualt-mock #'casual-org-table-info-formula-syntax)
            (casualt-mock #'casual-org-table-info-calc-functions)
            (casualt-mock #'casual-org-table-insert-calc-sum)
            (casualt-mock #'casual-org-table-insert-calc-mean)
            (casualt-mock #'casual-org-table-insert-calc-max)
            (casualt-mock #'casual-org-table-insert-calc-min)
            (casualt-mock #'undo)
            (casualt-mock #'transient-quit-all))

    (let ((test-vectors
           '((:binding "@<" :command casual-org-table-fedit-first-row-reference)
             (:binding "@>" :command casual-org-table-fedit-last-row-reference)
             (:binding "$<" :command casual-org-table-fedit-first-column-reference)
             (:binding "$>" :command casual-org-table-fedit-last-column-reference)
             (:binding "1" :command casual-org-table-fedit-first-hline-reference)
             (:binding "2" :command casual-org-table-fedit-second-hline-reference)
             (:binding "r" :command casual-org-table-fedit-hline-range-reference)
             (:binding "f" :command casual-org-table-info-calc-functions)
             (:binding "s" :command casual-org-table-insert-calc-sum)
             (:binding "m" :command casual-org-table-insert-calc-mean)
             (:binding "a" :command casual-org-table-insert-calc-max)
             (:binding "z" :command casual-org-table-insert-calc-min)
             (:binding "R" :command casual-org-table-info-references)
             (:binding "F" :command casual-org-table-info-formula-syntax)
             (:binding "U" :command undo :transient t)
             (:binding "RET" :command transient-quit-all))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-org-table-fedit-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-org-breakdown))


(provide 'test-casual-org)
;;; test-casual-org.el ends here
