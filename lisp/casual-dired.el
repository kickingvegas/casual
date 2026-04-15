;;; casual-dired.el --- Transient UI for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Charles Y. Choi

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

;; Casual Dired is an opinionated Transient-based user interface for Emacs Dired.

;; INSTALLATION
;; (require 'casual-dired) ; optional if using autoloaded menu
;; (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
;; (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
;; (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu) ; optional

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:
(require 'dired)
(require 'dired-x)
(require 'wdired)
(require 'image-dired)
(require 'casual-lib)
(require 'casual-dired-sort-by)

(require 'casual-dired-variables)
(require 'casual-dired-settings)
(require 'casual-dired-utils)

;;; Menus
;;;###autoload (autoload 'casual-dired-tmenu "casual-dired" nil t)
(transient-define-prefix casual-dired-tmenu ()
  "Transient menu for Dired."
  :refresh-suffixes t

  [:inapt-if-not-derived 'dired-mode
   ["File"
    ("o" "Open Other" dired-find-file-other-window)
    ("v" "View" dired-view-file)
    ("C" "Copy to…" dired-do-copy :transient t)
    ("R" "Rename…" dired-do-rename :transient t)
    ("D" "Delete…" dired-do-delete :transient t)
    ("l" "Link›" casual-dired-link-tmenu)
    ("c" "Change›" casual-dired-change-tmenu)
    ("y" "Type" dired-show-file-type :transient t)
    ("w" "Copy Name" dired-copy-filename-as-kill)
    ("!" "Shell…" dired-do-shell-command)
    ("&" "Shell &… " dired-do-async-shell-command)
    (";" "Thumbnail" image-dired-dired-toggle-marked-thumbs
     :if casual-dired-image-file-p
     :transient t)
    ("W" "Browse" browse-url-of-dired-file)]

   ["Directory"
    ("s" "Sort By›" casual-dired-sort-by-tmenu
     :if casual-dired-show-sort-by-tmenu-p
     :transient t)
    ("h" "Hide Details" dired-hide-details-mode
     :description
     (lambda ()
       (casual-lib-checkbox-label dired-hide-details-mode "Hide Details"))
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("O" "Omit Mode" dired-omit-mode
     :description
     (lambda () (casual-lib-checkbox-label dired-omit-mode "Omit Mode"))
     :transient t)
    ("i" "Insert Subdir" dired-maybe-insert-subdir
     :if casual-dired-directory-p
     :transient t)
    ("$" "Hide/Unhide Subdir" dired-hide-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :transient t)
    ("k" "Kill (Hide) Line(s)" dired-do-kill-lines
     :description (lambda () (if prefix-arg "Kill Subdir" "Kill (Hide) Line(s)✦"))
     :if casual-dired-marked-files-p
     :transient t)
    ("g" "Revert" revert-buffer :transient t)
    ("f" "Filter by name…" casual-dired-find-dired-regexp)
    ("E" "Edit (wdired)" wdired-change-to-wdired-mode)
    ("T" "Thumbnails…" image-dired :if display-graphic-p)
    ("d" "Dired…" dired)]

   ["Bulk"
    ("m" "Mark" dired-mark
     :if-not casual-dired-marked-p
     :transient t)
    ("u" "Unmark" dired-unmark
     :if casual-dired-marked-p
     :transient t)
    ("U" "Unmark All" dired-unmark-all-marks
     :if casual-dired-marked-files-p
     :transient t)
    ("t" "Toggle Marks" dired-toggle-marks :transient t)
    ("r" "Regexp›" casual-dired-regexp-tmenu)
    ("/" "Search & Replace›" casual-dired-search-replace-tmenu)
    ("#" "Utils›" casual-dired-utils-tmenu)
    ("~" "Flag Backups" dired-flag-backup-files :transient t)
    ("x" "Delete Flagged" dired-do-flagged-delete :transient t)]

   ["Navigation"
    :pad-keys t
    ("^" ".." dired-up-directory
     :description (lambda ()
                    (format ".. %s" (casual-dired-unicode-get :directory)))
     :transient t)

    ("p" " ↑ 📄" dired-previous-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)

    ("n" " ↓ 📄" dired-next-line
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-[" " ↑ *📄" dired-prev-marked-file
     :if casual-dired-marked-p
     :description (lambda ()
                    (format "%s %s%s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (char-to-string dired-marker-char)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-]" " ↓ *📄" dired-next-marked-file
     :if casual-dired-marked-p
     :description (lambda ()
                    (format "%s %s%s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (char-to-string dired-marker-char)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-p" " ↑ 📁" dired-prev-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :directory)))
     :transient t)
    ("M-n" " ↓ 📁" dired-next-dirline
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :directory)))
     :transient t)
    ("[" " ↑ 🗂️" dired-prev-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :up-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)
    ("]" " ↓ 🗂️" dired-next-subdir
     :if-not casual-dired-lisp-dired-buffer-p
     :description (lambda ()
                    (format "%s %s"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :down-arrow)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)
    ("j" " → 📄…" dired-goto-file
     :description (lambda ()
                    (format "%s %s…"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :goto)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :file)))
     :transient t)
    ("M-j" " → 🗂️…" dired-goto-subdir
     :description (lambda ()
                    (format "%s %s…"
                            (casual-dired-format-arrow
                             (casual-dired-unicode-get :goto)
                             casual-lib-use-unicode)
                            (casual-dired-unicode-get :subdir)))
     :transient t)]]

  [:inapt-if-not-derived 'dired-mode
   ["Quick"
    ("J" "Jump to Bookmark…" bookmark-jump)
    ("B" "Add Bookmark…" bookmark-set-no-overwrite)
    ("b" "List Buffers" ibuffer)]

   ["Search"
    :pad-keys t
    ("C-s" "Filename I-Search…" dired-isearch-filenames)
    ("M-s" "Filename I-Search Regexp…" dired-isearch-filenames-regexp)
    ("M-f" "Find in files…" rgrep)]

   ["New"
    ("+" "Directory" dired-create-directory :transient t)
    ("F" "File" dired-create-empty-file :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Open" dired-find-file)
   ("," "Settings›" casual-dired-settings-tmenu)
   ("q" "Quit Dired" quit-window)])

(transient-define-prefix casual-dired-regexp-tmenu ()
  "Transient menu for Dired regexp functions."
  :refresh-suffixes t
  ["Regexp"
   ["Mark"
    ("m" "Files…" dired-mark-files-regexp :transient t)
    ("c" "Files containing…" dired-mark-files-containing-regexp :transient t)
    ("u" "Unmark" dired-unmark
     :inapt-if-not casual-dired-marked-p
     :transient t)
    ("U" "Unmark All" dired-unmark-all-marks
     :inapt-if-not casual-dired-marked-files-p
     :transient t)]
   ["Operate"
    ("F" "Files to open…" dired-do-find-marked-files)
    ("C" "Files to copy…" dired-do-copy-regexp)
    ("r" "Files to rename…" dired-do-rename-regexp)
    ("D" "Files to delete…" dired-do-delete)]
   ["Flag"
    ("d" "Files for deletion…" dired-flag-files-regexp)
    ("x" "Delete flagged files" dired-do-flagged-delete)]]
  casual-lib-navigation-group-with-return)

(transient-define-prefix casual-dired-change-tmenu ()
  ["Change"
   [("M" "Mode…" dired-do-chmod :transient t)
    ("G" "Group…" dired-do-chgrp :transient t)
    ("O" "Owner…" dired-do-chown :transient t)]
   [("T" "Touch" dired-do-touch :transient t)]]
  casual-lib-navigation-group-with-return)

;;; Functions

(defun casual-dired-directory-p ()
  "Predicate if Dired item is a directory."
  (condition-case nil
      (and (file-directory-p (thing-at-point 'filename))
           (not (casual-dired-lisp-dired-buffer-p)))
    (error nil)))

(defun casual-dired-image-file-p ()
  "Predicate if Dired item is an image file."
  (if (and (display-graphic-p) (derived-mode-p 'dired-mode))
      (let ((fname (dired-file-name-at-point)))
        (if fname
            (string-match-p (image-file-name-regexp)
                            fname)
          nil))
    nil))

(defun casual-dired-lisp-dired-buffer-p ()
  "Predicate if buffer name is “*Find Lisp Dired*”.

This buffer is created by the command `find-lisp-find-dired'."
  (and (derived-mode-p 'dired-mode)
       (string-equal (buffer-name) "*Find Lisp Dired*")))

(defun casual-dired-show-sort-by-tmenu-p ()
  "Predicate to show `casual-dired-sort-by-tmenu'."
  (and (equal dired-use-ls-dired t)
       (not (casual-dired-lisp-dired-buffer-p))))

(defun casual-dired-find-dired-regexp (REGEXP)
  "Recursively find file names in current directory matching REGEXP.

Recursively find all files whose name matches the Elisp REGEXP
from the current directory `default-directory'. The value of
REGEXP will be interactively prompted for.

The command `find-lisp-find-dired' does all the heavy lifting
here.

* References
- Info node `(elisp) Regular Expressions'"
  (interactive "sFind filenames with regex: ")
  (find-lisp-find-dired default-directory REGEXP))

;;; Labels
(defun casual-dired-format-arrow (buf typeset)
  "If TYPESET is non-nil, then format BUF string to have space."
  (if typeset
      (format " %s" buf)
    buf))

(provide 'casual-dired)
;;; casual-dired.el ends here
