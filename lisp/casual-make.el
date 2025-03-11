;;; casual-make.el --- Transient UI for Make -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Y. Choi

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

;; This library provides a Transient-based user interface for `make-mode'.

;;
;; INSTALLATION

;; In your initialization file, bind the Transient `casual-make-tmenu' to your
;; key binding of preference. Two suggested bindings are 'M-m' and 'C-c m'.

;; (require 'casual-make) ; optional if using autoloaded menu
;; (keymap-set makefile-mode-map "M-m" #'casual-make-tmenu)

;;; Code:
(require 'casual-make-utils)
(require 'casual-make-settings)

;;;###autoload (autoload 'casual-make-tmenu "casual-make" nil t)
(transient-define-prefix casual-make-tmenu ()
  "Main menu for Casual Make.

This menu provides a user interface for the commands in `make-mode'."

  ["Casual Make"
   :description (lambda () (format "Casual Make (%s)" (casual-make-mode-label major-mode)))
   ["Edit"
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("\\" "Backslash region" makefile-backslash-region :inapt-if-not use-region-p)
    (";" "Comment region" comment-region :inapt-if-not use-region-p)
    (":" "Insert target…" makefile-insert-target-ref)
    ("m" "Insert macro…" makefile-insert-macro-ref)
    ("f" "Insert GNU function…" makefile-insert-gmake-function)
    ("a" "Automatic Variables›" casual-make-automatic-variables-tmenu)]

   ["Pickup as targets"
    ("E" "Everything" makefile-pickup-everything)
    ("F" "Filenames" makefile-pickup-filenames-as-targets)]

   ["Misc"
    ("c" "Compile…" compile)
    ("o" "Overview" makefile-create-up-to-date-overview)
    ("t" "Makefile Type›" casual-make-mode-select-tmenu :transient t)
    ("." "Identify Auto Var" casual-make-identify-autovar-region
     :inapt-if-not use-region-p)]

   ["Navigate"
    ("i" "Index Menu…" imenu :transient t)
    ("p" "Previous" makefile-previous-dependency
     :description (lambda ()
                    (format "%s target" (casual-make-unicode-get :previous)))
     :transient t)
    ("n" "Next" makefile-next-dependency
     :description (lambda ()
                    (format "%s target"(casual-make-unicode-get :next)))
     :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-make-settings-tmenu)
   ("I" "ⓘ Make" (lambda () (interactive) (info "(make) Top")))
   (casual-lib-quit-all)
   ("RET" "Exit Menu" transient-quit-all)])

(provide 'casual-make)
;;; casual-make.el ends here
