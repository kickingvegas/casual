;;; cc-dired-sort-by.el --- Dired Sort By            -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: unix, tools

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

;; casual-dired-sort-by is a set of functions that provide richer sorting
;; capability to Dired mode. Two means are provided:
;;
;; - Transient menu for a keyboard-driven workflow
;; - Context menu for a mouse-driven workflow
;;
;; * Installation
;;
;; To use the Transient menu, ensure that you have these statements in your
;; init file.
;;
;; (require 'casual-dired-sort-by)
;; (define-key dired-mode-map (kbd "s") 'casual-dired-sort-by-tmenu)
;;
;; The keymap `casual-dired-sort-menu' can be used in a context menu
;; (see `context-menu-mode') or some other main menu.
;;
;; Typically this is done with code looking like this:

;; (defun casual-dired-context-menu-addons (menu click)
;;   "Customize context MENU with CLICK event."
;;   (easy-menu-add-item menu nil casual-dired-sort-menu)
;;   menu)
;; (add-hook 'context-menu-functions #'casual-dired-context-menu-addons)

;;; Code:
(require 'dired)
(require 'easymenu)
(require 'mouse)
(require 'casual-lib)
(require 'casual-dired-utils)
(require 'casual-dired-variables)

;;;###autoload (autoload 'casual-dired-sort-by-tmenu "casual-dired-sort-by" nil t)
(transient-define-prefix casual-dired-sort-by-tmenu ()
  "Transient menu to sort Dired buffer by different criteria.

This function requires GNU ls from coreutils installed."
  :value casual-dired-listing-switches
  :man-page "ls"
  [["Options"
    ("." "Show all (include .*)" "--all")
    ("g" "Group directories first" "--group-directories-first")
    ("r" "Reverse" "--reverse")
    ("t" "Set time style" "--time-style="
     :choices ("full-iso" "long-iso" "iso" "locale"))
    ("h" "Show size like 1K 234M 2G etc." "--human-readable")
    ("1" "Like above but in powers of 1000" "--si")
    ("O" "Hide owner" "-g")
    ("G" "Hide group" "--no-group")
    ("N" "Use numeric UID/GID" "--numeric-uid-gid")
    ("i" "Show inode" "--inode")]

   ["Sort By"
    ("n" "Name" casual-dired--sort-by-name :transient nil)
    ("k" "Kind" casual-dired--sort-by-kind :transient nil)
    ("l" "Date Last Opened" casual-dired--sort-by-date-last-opened
     :transient nil)
    ("a" "Date Added" casual-dired--sort-by-date-added :transient nil)
    ("m" "Date Modified" casual-dired--sort-by-date-modified :transient nil)
    ("M" "Date Metadata Changed" casual-dired--sort-by-date-metadata-changed
     :transient nil)

    ("s" "Size" casual-dired--sort-by-size :transient nil)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(defun casual-dired--sort-by-name ()
  "Sort directory by name.

This command passes the appropriate arguments to GNU ls to sort
the directory by name."
  (interactive)
  (casual-dired--sort-by
   :name
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-kind ()
  "Sort directory by kind.

This command passes the appropriate arguments to GNU ls to sort
the directory by kind."
  (interactive)
  (casual-dired--sort-by
   :kind
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-date-last-opened ()
  "Sort directory by date last opened.

This command passes the appropriate arguments to GNU ls to sort
the directory by date last opened."
  (interactive)
  (casual-dired--sort-by
   :date-last-opened
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-date-added ()
  "Sort directory by date added.

This command passes the appropriate arguments to GNU ls to sort
the directory by date added."
  (interactive)
  (casual-dired--sort-by
   :date-added
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-date-modified ()
  "Sort directory by date modified.

This command passes the appropriate arguments to GNU ls to sort
the directory by date modified."
  (interactive)
  (casual-dired--sort-by
   :date-modified
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-date-metadata-changed ()
  "Sort directory by date metadata changed.

This command passes the appropriate arguments to GNU ls to sort
the directory by date metadata changed."
  (interactive)
  (casual-dired--sort-by
   :date-metadata-changed
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-version ()
  "Sort directory by version.

This command passes the appropriate arguments to GNU ls to
naturally sort the directory with respect to the numbers in the
filename.  For example, starting with name sorting, a version
sort would have the following result:

foo10.txt                      foo1.txt
foo1.txt   -> version sort ->  foo2.txt
foo2.txt                       foo10.txt

Note that the argument ‘--group-directories-first’ must not be
enabled for this command to work."
  (interactive)
  (casual-dired--sort-by
   :version
   (transient-args transient-current-command)))

(defun casual-dired--sort-by-size ()
  "Sort directory by size."
  (interactive)
  (casual-dired--sort-by
   :size
   (transient-args transient-current-command)))

(defun casual-dired--sort-by (criteria &optional prefix-args)
  "Sort current Dired buffer according to CRITERIA and PREFIX-ARGS.

This function will invoke `dired-sort-other' with arguments built from
CRITERIA and PREFIX-ARGS.

CRITERIA is a keyword of which the following are supported:
  :name             :date-added             :version
  :kind             :date-metadata-changed  :size
  :date-last-opened :date-modified

PREFIX-ARGS is a list of GNU ls arguments. If nil, then it will use the value
of `casual-dired-listing-switches'. Otherwise this is typically populated by the
Transient menu `casual-dired-sort-by-tmenu'.

This function requires GNU ls from coreutils installed.

See the man page `ls(1)' for details."
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (let ((arg-list (list "-l")))
    (if prefix-args
        (nconc arg-list prefix-args)
      (nconc arg-list casual-dired-listing-switches))
    (cond
     ((eq criteria :name)
      (message "Sorted by name"))

     ((eq criteria :kind)
      (message "Sorted by kind")
      (push "--sort=extension" arg-list))

     ((eq criteria :date-last-opened)
      (message "Sorted by date last opened")
      (push "--sort=time" arg-list)
      (push "--time=access" arg-list))

     ((eq criteria :date-added)
      (message "Sorted by date added")
      (push "--sort=time" arg-list)
      (push "--time=creation" arg-list))

     ((eq criteria :date-modified)
      (message "Sorted by date modified")
      (push "-t" arg-list))

     ((eq criteria :date-metadata-changed)
      (message "Sorted by date metadata changed")
      (push "--sort=time" arg-list)
      (push "--time=status" arg-list))

     ((eq criteria :version)
      (message "Sorted by version")
      (push "--sort=version" arg-list))

     ((eq criteria :size)
      (message "Sorted by size")
      (push "-S" arg-list))

     (t
      (message "Default sorted by name")))

    (dired-sort-other (mapconcat #'identity arg-list " "))
    (casual-dired--update-mode-name criteria (member "--reverse" arg-list))))

(defun casual-dired--update-mode-name (criteria reverse)
  "Update Dired `mode-name' given CRITERIA and REVERSE.

Update the value of `mode-name' as part of the sort operation
done by `casual-dired-sort-by'.

- CRITERIA: sort criteria key
- REVERSE: if non-nil then buffer sorted with the --reverse argument"
  (unless (derived-mode-p 'dired-mode)
    (error "Not in Dired mode"))

  (setq mode-name (casual-dired--mode-name-from-sort criteria reverse))
  (force-mode-line-update))

(defun casual-dired--mode-name-from-sort (criteria reverse)
  "Generate Dired `mode-name' given CRITERIA and REVERSE.

Generate the value of `mode-name' as part of the sort operation
done by `casual-dired-sort-by'.

- CRITERIA: sort criteria key
- REVERSE: if non-nil then --reverse argument is used"
  (let* ((invert-reverse (member criteria '(:date-last-opened
                                            :date-added
                                            :date-modified
                                            :date-metadata-changed
                                            :size)))
         (reverse-icon (if (xor reverse invert-reverse) "↑" "↓"))
         (base-name "Dired")
         (template (concat base-name ": %s " reverse-icon))
         (new-mode-name (list)))

    (cond
     ((eq criteria :name) (push (format template "name") new-mode-name))
     ((eq criteria :kind) (push (format template "kind") new-mode-name))
     ((eq criteria :date-last-opened) (push (format template "last") new-mode-name))
     ((eq criteria :date-added) (push (format template "added") new-mode-name))
     ((eq criteria :date-modified) (push (format template "modified") new-mode-name))
     ((eq criteria :date-metadata-changed) (push (format template "Δ metadata") new-mode-name))
     ((eq criteria :version) (push (format template "version") new-mode-name))
     ((eq criteria :size) (push (format template "size") new-mode-name))
     (t (push base-name new-mode-name)))

    new-mode-name))

(easy-menu-define casual-dired-sort-menu nil
  "Keymap for Dired sort by menu."
  '("Sort By"
    :visible (and (derived-mode-p 'dired-mode) (not dired-sort-inhibit))
    ["Name"
     (lambda () (interactive) (casual-dired--sort-by :name))
     :help "Sort by name"]
    ["Kind"
     (lambda () (interactive) (casual-dired--sort-by :kind))
     :help "Sort by kind"]
    ["Date Last Opened"
     (lambda () (interactive) (casual-dired--sort-by :date-last-opened))
     :help "Sort by date last opened"]
    ["Date Added"
     (lambda () (interactive) (casual-dired--sort-by :date-added))
     :help "Sort by date added"]
    ["Date Modified"
     (lambda () (interactive) (casual-dired--sort-by :date-modified))
     :help "Sort by date modified"]
    ["Date Metadata Changed"
     (lambda () (interactive) (casual-dired--sort-by :date-metadata-changed))
     :help "Sort by date metadata changed"]
    ["Version"
     (lambda () (interactive) (casual-dired--sort-by :version))
     :help "Sort by version"]
    ["Size"
     (lambda () (interactive) (casual-dired--sort-by :size))
     :help "Sort by size"]
    "--"
    ("Reverse Sort By"
     ["Name"
      (lambda () (interactive) (casual-dired--sort-by
                                :name
                                (append '("--reverse") cc-dired-listing-switches)))

      :help "Reverse sort by name"]
     ["Kind"
      (lambda () (interactive) (casual-dired--sort-by
                                :kind
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by kind"]
     ["Date Last Opened"
      (lambda () (interactive) (casual-dired--sort-by
                                :date-last-opened
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date last opened"]
     ["Date Added"
      (lambda () (interactive) (casual-dired--sort-by
                                :date-added
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date added"]
     ["Date Modified"
      (lambda () (interactive) (casual-dired--sort-by
                                :date-modified
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date modified"]
     ["Date Metadata Changed"
      (lambda () (interactive) (casual-dired--sort-by
                                :date-metadata-changed
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date metadata changed"]
     ["Version"
      (lambda () (interactive) (casual-dired--sort-by
                                :version
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by version"]
     ["Size"
      (lambda () (interactive) (casual-dired--sort-by
                                :size
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by size"])))

(provide 'casual-dired-sort-by)
;;; casual-dired-sort-by.el ends here
