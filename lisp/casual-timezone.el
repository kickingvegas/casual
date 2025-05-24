;;; casual-timezone.el --- Timezone Planner  -*- lexical-binding: t; -*-

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

(require 'casual-timezone-settings)

;;;###autoload (autoload 'casual-timezone-tmenu "casual-timezone" nil t)
(transient-define-prefix casual-timezone-tmenu ()
  "Main menu for Casual Timezone."

  ["Casual Timezone"
   ("l" "Local to Remote…" casual-timezone-local-time-to-remote)
   ("r" "Remote to Local…" casual-timezone-remote-time-to-local)
   ("z" "Planner…" casual-timezone-planner)]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-timezone-settings-tmenu)
   (casual-lib-quit-all)])

(provide 'casual-timezone)
;;; casual-timezone.el ends here
