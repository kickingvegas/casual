;;; casual-$MODULE.el --- Transient UI for $MODULE -*- lexical-binding: t; -*-

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

;; This library provides a Transient-based user interface for `$MODULE-mode'.

;; INSTALLATION

;; In your initialization file, bind the Transient `casual-$MODULE-tmenu' to your
;; key binding of preference.

;; (require 'casual-$MODULE) ; optional if using autoloaded menu
;; (keymap-set $MODULE-mode-map "C-o" #'casual-$MODULE-tmenu)

;;; Code:
(require 'casual-$MODULE-settings)
(require 'casual-$MODULE-utils)

;;;###autoload (autoload 'casual-$MODULE-tmenu "casual-$MODULE" nil t)
(transient-define-prefix casual-$MODULE-tmenu ()
  "Casual $MODULE main menu."


  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-$MODULE-settings-tmenu)
   ;; ("I" "ⓘ" casual-$MODULE-info)
   ("q" "Quit" quit-window)
   (casual-lib-quit-all)])

(provide 'casual-$MODULE)
;;; casual-$MODULE.el ends here
