;;; casual-image-settings.el --- Casual Image Settings  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'image-mode)
(require 'cus-edit)
(require 'casual-lib)

(transient-define-prefix casual-image-settings-tmenu ()
  ["Casual Image Settings"
   ["Customize"
    ("G" "Image Group" casual-image--customize-group)]

   ["Casual"
    (casual-lib-customize-unicode)
    (casual-lib-customize-hide-navigation)]]

  [:class transient-row
   (casual-lib-quit-one)
   ;; ("a" "About" casual-info-about :transient nil)
   (casual-lib-quit-all)])

(defun casual-image--customize-group ()
  "Customize Image group."
  (interactive)
  (customize-group "image"))

(provide 'casual-image-settings)
;;; casual-image-settings.el ends here
