;;; casual-make-utils.el --- Casual Make Utils -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Charles Y. Choi

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
(require 'map)
(require 'make-mode)
(require 'casual-lib)

(defconst casual-make-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))

  "Unicode symbol DB to use for Make Transient menus.")

(defun casual-make-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-make-unicode-db))


(defconst casual-make--make-mode-label-map
  '((makefile-automake-mode . "automake")
    (makefile-bsdmake-mode . "BSD make")
    (makefile-gmake-mode . "GNU make")
    (makefile-imake-mode . "imake")
    (makefile-mode . "make")
    (makefile-makepp-mode . "makepp"))
  "Map of make modes to their labels.")

(defun casual-make-mode-label (mode)
  "Label for make MODE."
  (map-elt casual-make--make-mode-label-map mode))

(transient-define-prefix casual-make-mode-select-tmenu ()
  "Configure `make-mode' for different variants of Make."

  ["Select Make Mode"
   :description (lambda ()
                  (format
                   "Select Make Mode (%s)"
                   (casual-make-mode-label major-mode)))
   [("a" "automake" makefile-automake-mode)
    ("b" "BSD make" makefile-bsdmake-mode)
    ("g" "GNU make" makefile-gmake-mode)]
   [("i" "imake" makefile-imake-mode)
    ("m" "make" makefile-mode)
    ("e" "makepp" makefile-makepp-mode)]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(defconst casual-make--autovar-description-map
  '(("$@" . "The target name.")
    ("$(@D)" . "The directory part of the target.")
    ("$(@F)" . "The file-within-directory part of the target.")
    ("$*" . "Stem with which an implicit rule matches.")
    ("$(*D)" . "Directory part of stem.")
    ("$(*F)" . "File-within-directory part of stem.")
    ("$%" . "The target member name, when the target is an archive member.")
    ("$(%D)" . "The directory part of the target archive member name.")
    ("$(%F)" . "The file-within-directory part of the target archive member name.")
    ("$<" . "The name of the first prerequisite.")
    ("$(<D)" . "The directory part of the first prerequisite.")
    ("$(<F)" . "The file-within-directory part of the first prerequisite.")
    ("$?" . "The names of all the prerequisites that are newer than the target.")
    ("$(?D)" . "Lists of the directory parts of all prerequisites newer than the target.")
    ("$(?F)" . "Lists of the file-within-directory parts of all prerequisites newer than the target.")
    ("$^" . "The names of all prerequisites, normalized (de-duplicated).")
    ("$(^D)" . "Lists of the directory parts of all prerequisites, normalized.")
    ("$(^F)" . "Lists of the file-within-directory parts of all prerequisites, normalized.")
    ("$+" . "The names of all prerequisites, allowing duplicates.")
    ("$(+D)" . "Lists of the directory parts of all prerequisites, allowing duplicates.")
    ("$(+F)" . "Lists of the file-within-directory parts of all prerequisites, allowing duplicates."))
  "Map of automatic variable descriptions.")

(defun casual-make--identify-autovar (value)
  "Identify GNU Make automatic variable in VALUE."
  (let* ((value (string-clean-whitespace value))
         (description (map-elt casual-make--autovar-description-map value)))
    (if description
        (format "%s - %s" value description)
      (format "%s is not a GNU Make automatic variable." value))))

(defun casual-make-identify-autovar-region (start end)
  "Identify GNU Make automatic variable in region from START to END."
  (interactive "r")
  (let ((value (buffer-substring start end)))
    (message (casual-make--identify-autovar value))))


;; Transients
(transient-define-prefix casual-make-automatic-variables-tmenu ()
  "Makefile automatic variables menu.

Menu for GNU Make automatic variables.

For more info, refer to info node `(make) Automatic Variables'."

  ["Automatic Variables - GNU Make"
   ["Target @"
    :pad-keys t
    ("$@" "Name"
     (lambda ()
       "The target name."
       (interactive) (insert "$@")))
    ("$(@D)" "Directory"
     (lambda ()
       "The directory part of the target."
       (interactive) (insert "$(@D)")))
    ("$(@F)" "File"
     (lambda ()
       "The file-within-directory part of the target."
       (interactive) (insert "$(@F)")))]

   ["Implicit Stem *"
    :pad-keys t
    ("$*" "Stem"
     (lambda ()
       "Stem with which an implicit rule matches."
       (interactive) (insert "$*")))
    ("$(*D)" "Directory"
     (lambda ()
       "Directory part of stem."
       (interactive) (insert "$(*D)")))
    ("$(*F)" "File"
     (lambda ()
       "File-within-directory part of stem."
       (interactive) (insert "$(*F)")))]

   ["Archive %"
    :pad-keys t
    ("$%" "Archive"
     (lambda ()
       "The target member name, when the target is an archive member."
       (interactive) (insert "$%")))
    ("$(%D)" "Directory"
     (lambda ()
       "The directory part of the target archive member name."
       (interactive) (insert "$(%D)")))
    ("$(%F)" "File"
     (lambda ()
       "The file-within-directory part of the target archive member name."
       (interactive) (insert "$(%F)")))]]

  ["Prerequisites"
   ["First <"
    :pad-keys t
    ("$<" "First"
     (lambda ()
       "The name of the first prerequisite."
       (interactive) (insert "$<")))
    ("$(<D)" "Directory"
     (lambda ()
       "The directory part of the first prerequisite."
       (interactive) (insert "$(<D)")))
    ("$(<F)" "File"
     (lambda ()
       "The file-within-directory part of the first prerequisite."
       (interactive) (insert "$(<F)")))]

   ["Newer than Target ?"
    :pad-keys t
    ("$?" "All"
     (lambda ()
       "The names of all the prerequisites that are newer than the target."
       (interactive) (insert "$?")))
    ("$(?D)" "Directory"
     (lambda ()
       "Lists of the directory parts of all prerequisites newer than the target."
       (interactive) (insert "$(?D)")))
    ("$(?F)" "File"
     (lambda ()
       "Lists of the file-within-dir parts of all prerequisites newer than the target."
       (interactive) (insert "$(?F)")))]

   ["Normalized ^"
    :pad-keys t
    ("$^" "All"
     (lambda ()
       "The names of all prerequisites, normalized (de-duplicated)."
       (interactive) (insert "$^")))
    ("$(^D)" "Directory"
     (lambda ()
       "Lists of the directory parts of all prerequisites, normalized."
       (interactive) (insert "$(^D)")))
    ("$(^F)" "File"
     (lambda ()
       "Lists of the file-within-directory parts of all prerequisites, normalized."
       (interactive) (insert "$(^F)")))]

   ["Include Duplicates +"
    :pad-keys t
    ("$+" "All"
     (lambda ()
       "The names of all prerequisites, allowing duplicates."
       (interactive) (insert "$+")))
    ("$(+D)" "Directory"
     (lambda ()
       "Lists of the directory parts of all prerequisites, allowing duplicates."
       (interactive) (insert "$(+D)")))
    ("$(+F)" "File"
     (lambda ()
       "Lists of the file-within-dir parts of all prerequisites, allowing duplicates."
       (interactive) (insert "$(+F)")))]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("RET" "Dismiss" casual-lib-quit-all)
   ("i" "Info" (lambda () (interactive) (info "(make) Automatic Variables")))])

(provide 'casual-make-utils)
;;; casual-make-utils.el ends here
