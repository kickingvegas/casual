;;; casual-bookmarks.el --- Transient UI for Bookmarks -*- lexical-binding: t; -*-

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

;; Casual Bookmarks is an opinionated Transient-based user interface for Emacs Bookmarks.

;; INSTALLATION
;; (require 'casual-bookmarks) ; optional if using autoloaded menu
;; (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'bookmark)
(require 'transient)
(require 'tabulated-list)
(require 'casual-lib)
(require 'casual-bookmarks-utils)
(require 'casual-bookmarks-settings)

;;;###autoload (autoload 'casual-bookmarks-tmenu "casual-bookmarks" nil t)
(transient-define-prefix casual-bookmarks-tmenu ()
  "CC Bookmark Transient menu."

  ["Bookmarks"
   ["Operations"
    ("RET" "Open" bookmark-bmenu-this-window :transient nil)
    ("o" "Open other" bookmark-bmenu-other-window :transient nil)
    ("v" "Open marked"  bookmark-bmenu-select :transient nil)
    ("r" "Rename…" bookmark-bmenu-rename :transient nil)
    ("R" "Relocate…" bookmark-bmenu-relocate :transient nil)
    ("x" "Delete marked D" bookmark-bmenu-execute-deletions :transient t)
    ("+" "Add…" casual-bookmarks-add-bookmark-via-buffer :transient t)
    ("w" "Show location" bookmark-bmenu-locate :transient t)]

   ["Mark"
    ("m" "Mark" bookmark-bmenu-mark :transient t)
    ("M" "Mark all" bookmark-bmenu-mark-all :transient t)
    ("u" "Unmark" bookmark-bmenu-unmark :transient t)
    ("U" "Unmark all" bookmark-bmenu-unmark-all :transient t)
    ("d" "Delete" bookmark-bmenu-delete :transient t)
    ("D" "Delete all" bookmark-bmenu-delete-all :transient t)]

   ["Display"
    ("S" "Sort by›" casual-bookmarks-sortby-tmenu :transient t)
    ("t" "Toggle locations" bookmark-bmenu-toggle-filenames :transient t)
    ("/" "Filter…" bookmark-bmenu-search :transient t)
    ("g" "Refresh" revert-buffer :transient t)]

   ["Annotation"
    ("e" "Edit" bookmark-bmenu-edit-annotation :transient nil)
    ("a" "Show" bookmark-bmenu-show-annotation :transient nil)
    ("A" "Show All" bookmark-bmenu-show-all-annotations :transient nil)]

   ["Navigate"
    ("p" "↑" previous-line
     :description (lambda () (casual-bookmarks-unicode-get :previous))
     :transient t)
    ("n" "↓" next-line
     :description (lambda () (casual-bookmarks-unicode-get :next))
     :transient t)
    ("<" "⤒" beginning-of-buffer
     :description (lambda () (casual-bookmarks-unicode-get :beginning-of-buffer))
     :transient t)
    (">" "⤓" end-of-buffer
     :description (lambda () (casual-bookmarks-unicode-get :end-of-buffer))
     :transient t)
    ("J" "🚀…" bookmark-jump
     :description (lambda ()
                    (format "%s…" (casual-bookmarks-unicode-get :jump)))
     :transient nil)]]

  ["Column"
   :class transient-row
   ("b" "←" tabulated-list-previous-column
    :description (lambda () (casual-bookmarks-unicode-get :backward))
    :transient t)
   ("f" "→" tabulated-list-next-column
    :description (lambda () (casual-bookmarks-unicode-get :forward))
    :transient t)
   ("{" "→←" tabulated-list-narrow-current-column
    :description (lambda () (casual-bookmarks-unicode-get :narrow))
    :transient t)
   ("}" "←→" tabulated-list-widen-current-column
    :description (lambda () (casual-bookmarks-unicode-get :widen))
    :transient t)]

  [:class transient-row
          (casual-lib-quit-one)
          ;;("l" "Load…" bookmark-bmenu-load :transient t)
          ("s" "Save" bookmark-bmenu-save :transient t)
          ("," "Settings›" casual-bookmarks-settings-tmenu)
          (casual-lib-quit-all)
          ("q" "Quit" quit-window)])

(transient-define-prefix casual-bookmarks-sortby-tmenu ()
  ["Bookmarks: Sort By"
   ("n" "Name" casual-bookmarks-sortby-name :transient nil)
   ("m" "Last Modified" casual-bookmarks-sortby-modified :transient nil)
   ("c" "Creation Time" casual-bookmarks-sortby-creation :transient nil)]
  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(defun casual-bookmarks-add-bookmark-via-buffer (buffer)
  "Set a bookmark for an interactively selected BUFFER."
  (interactive "B")
  (with-current-buffer buffer
    (bookmark-set)))

(defun casual-bookmarks-sortby-name ()
  "Sort bookmark list by name."
  (interactive)
  (setq-local bookmark-sort-flag t)
  (revert-buffer))

(defun casual-bookmarks-sortby-modified ()
  "Sort bookmark list by last modified time."
  (interactive)
  (setq-local bookmark-sort-flag 'last-modified)
  (revert-buffer))

(defun casual-bookmarks-sortby-creation ()
  "Sort bookmark list by creation time."
  (interactive)
  (setq-local bookmark-sort-flag nil)
  (revert-buffer))

;; Streamlined Bookmarks Menu
(easy-menu-define casual-bookmarks-main-menu nil
  "Keymap for CC Bookmarks Menu."
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    "---"
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    "---"
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(provide 'casual-bookmarks)
;;; casual-bookmarks.el ends here
