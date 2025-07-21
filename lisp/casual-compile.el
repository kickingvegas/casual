;;; casual-compile.el --- Transient UI for Compilation Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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

;; This library provides a Transient-based user interface for
;; `compilation-mode'.

;; INSTALL

;; In your initialization file, bind the Transient `casual-compile-tmenu' to
;; your key binding of preference.

;; (keymap-set compilation-mode-map "C-o" #'casual-compile-tmenu)

;; The following keybindings are recommended to support consistent behavior
;; between `compilation-mode-map' and `casual-compile-tmenu'.

;; (keymap-set compilation-mode-map "k" #'compilation-previous-error)
;; (keymap-set compilation-mode-map "j" #'compilation-next-error)
;; (keymap-set compilation-mode-map "o" #'compilation-display-error)
;; (keymap-set compilation-mode-map "[" #'compilation-previous-file)
;; (keymap-set compilation-mode-map "]" #'compilation-next-file)

;;; Code:
(require 'casual-compile-settings)
(require 'casual-compile-utils)

(transient-define-prefix casual-compile-tmenu ()
  "Casual main menu for `compilation-mode'."
  :refresh-suffixes t
  ["Casual Compile"
   :description (lambda ()
                  (format "%s Results"
                          (casual-compile--select-mode-label "Compilation"
                                                             "Grep")))
   ["Follow"
    ("p" "Previous" previous-error-no-select
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("n" "Next" next-error-no-select
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)]

   ["Item"
    :description (lambda () (casual-compile--select-mode-label "Error"
                                                               "Match"))
    :pad-keys t
    ("k" "Previous" compilation-previous-error
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("j" "Next" compilation-next-error
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)
    ("o" "Display" compilation-display-error
     :description (lambda () (casual-compile-unicode-get :display))
     :transient t)
    ("RET" "Goto" compile-goto-error
     :description (lambda () (casual-compile-unicode-get :goto)))]

   ["File"
    ("[" "Previous" compilation-previous-file
     :description (lambda () (casual-compile-unicode-get :previous))
     :transient t)
    ("]" "Next" compilation-next-file
     :description (lambda () (casual-compile-unicode-get :next))
     :transient t)]

   ["Compile"
    :description (lambda ()
                   (casual-compile--select-mode-label
                    "Compile"
                    "Refresh"))
    ("g" "Recompile" recompile
     :description (lambda ()
                    (casual-compile--select-mode-label
                     "Recompile"
                     (casual-compile-unicode-get :refresh)))

     :transient t)
    ("c" "Compile" compile
     :if-not (lambda () (derived-mode-p 'grep-mode)))
    ("k" "Kill" kill-compilation
     :description (lambda () (casual-compile-unicode-get :kill))
     :transient t
     :if casual-compile--compilation-running-p)
    ]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("," "Settings›" casual-compile-settings-tmenu)
   ("q" "Quit" quit-window)])

(provide 'casual-compile)
;;; casual-compile.el ends here
