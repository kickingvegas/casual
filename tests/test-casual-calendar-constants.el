(require 'ert);;; test-casual-calendar-constants.el --- Casual Calendar Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Y. Choi

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


(require 'casual-lib-test-utils)
(require 'casual-calendar-test-utils)
(require 'casual-calendar)

(defun casualt-calendar-unicode-assert (key control)
 (casualt-unicode-db-assert key control #'casual-calendar-unicode-get))

(ert-deftest test-casual-calendar-unicode-get ()
  (let ((casual-lib-use-unicode nil))
    (casualt-calendar-unicode-assert :previous "Previous")
    (casualt-calendar-unicode-assert :next "Next")
    (casualt-calendar-unicode-assert :behind "Behind")
    (casualt-calendar-unicode-assert :ahead "Ahead")
    (casualt-calendar-unicode-assert :beginning "Beginning")
    (casualt-calendar-unicode-assert :end "End")
    (casualt-calendar-unicode-assert :back-3-months "-3 months")
    (casualt-calendar-unicode-assert :forward-3-months "+3 months")
    (casualt-calendar-unicode-assert :redraw "Refresh")
    (casualt-calendar-unicode-assert :goto "Goto")
    (casualt-calendar-unicode-assert :sunrise "Sunrise")
    (casualt-calendar-unicode-assert :lunar "Lunar"))

  (let ((casual-lib-use-unicode t))
    (casualt-calendar-unicode-assert :previous "↑")
    (casualt-calendar-unicode-assert :next "↓")
    (casualt-calendar-unicode-assert :behind "←")
    (casualt-calendar-unicode-assert :ahead "→")
    (casualt-calendar-unicode-assert :beginning "⇤")
    (casualt-calendar-unicode-assert :end "⇥")
    (casualt-calendar-unicode-assert :back-3-months "← 3 months")
    (casualt-calendar-unicode-assert :forward-3-months "→ 3 months")
    (casualt-calendar-unicode-assert :redraw "⟳")
    (casualt-calendar-unicode-assert :goto "🔎")
    (casualt-calendar-unicode-assert :sunrise "🌅")
    (casualt-calendar-unicode-assert :lunar "🌙")))

(provide 'test-casual-calendar-constants)
;;; test-casual-calendar-constants.el ends here
