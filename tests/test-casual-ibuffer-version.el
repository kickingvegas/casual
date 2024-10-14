;;; test-casual-ibuffer-version.el --- Casual IBuffer Version Tests  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'ert)
(require 'casual-ibuffer-test-utils)
(require 'casual-ibuffer-version)

(ert-deftest test-casual-ibuffer-version ()
  (should (stringp (casual-ibuffer-version)))
  (should (stringp casual-ibuffer-version)))

(provide 'test-casual-ibuffer-version)
;;; test-casual-ibuffer-version.el ends here
