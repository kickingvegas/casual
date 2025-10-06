;;; casual-timezone-utils.el --- Casual Timezone Utils -*- lexical-binding: t; -*-

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
(require 'org)
(require 'vtable)
(require 'casual-lib)
(require 'iso8601)

(defconst casual-timezone-unicode-db
  '((:previous . '("↑" "Previous"))
    (:next . '("↓" "Next"))
    (:forward . '("→" "Forward"))
    (:backward . '("←" "Backward"))
    (:right . '("→" "Right"))
    (:left . '("←" "Left"))
    (:current . '("⨀" "Current Hour")))

  "Unicode symbol DB to use for Timezone Transient menus.")

(defun casual-timezone-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-timezone-unicode-db))

(defcustom casual-timezone-working-hours-range '((:start . 9)(:stop . 17))
  "Working hours range.
The range of hour values are between 0 to 23, inclusive."
  :type '(alist :key-type symbol :value-type natnum)
  :group 'casual)

(defcustom casual-timezone-convert-datestamp-format "%Y-%m-%d %H:%M:%S %Z"
  "Datestamp format used for timezone conversion.

This customizable variable determines the reporting format used by the
commands `casual-timezone-local-time-to-remote' and
`casual-timezone-remote-time-to-local'.

The specification of this variable conforms to the format string used by
`format-time-string' as described in Info node `(elisp) Time Parsing'."
  :type 'string
  :group 'casual)

(defcustom casual-timezone-datestamp-format "%a %b %-e %Y, %l:%M %p"
  "Datestamp format used by `casual-timezone-planner'.

This customizable variable determines the reporting format used
by the command `casual-timezone-planner'.  The specification of this
variable conforms to the format string used by
`format-time-string' as described in Info node `(elisp) Time
Parsing'.

If 24 hour clock time is preferred, use ‘%k’ instead of ‘%l’."
  :type 'string
  :group 'casual)

(defcustom casual-timezone-working-hour-glyph "☼"
  "Working hour glyph used by `casual-timezone-planner'.

This customizable variable contains the glyph used to annotate a
working hour in `casual-timezone-planner'."
  :type 'string
  :group 'casual)

(defcustom casual-timezone-zone-info-database "/usr/share/zoneinfo/tzdata.zi"
  "Path to the tzdata.zi file used by `casual-timezone-zone-info'."
  :type 'file
  :group 'casual)

(defface casual-timezone-planner-working-highlight
  '((((type tty) (class color))
     :background "gray25")
    (((class color) (min-colors 88) (background light))
     :background "#FDFEB1" :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :background "gray25")
    (((class color) (min-colors 16) (background light))
     :background "yellow")
    (((class color) (min-colors 16) (background dark))
     :background "olive")
    (((class color) (min-colors 8))
     :background "olive" :foreground "black")
    (t :inverse-video t))
  "Casual Timezone Planner working hours highlight."
  :group 'casual)

(defun casual-timezone-zone-info ()
  "List of timezones in zoneinfo database.

This function reads the local zoneinfo database to obtain the
list of timezones.

This function requires that the zoneinfo database in
`casual-timezone-zone-info-database' exists and that awk is
installed."
  (unless (not (eq system-type 'windows-nt))
    (error "Not available on Windows"))

  (with-temp-buffer
    (call-process
     "awk"
     nil
     (current-buffer)
     nil
     "/^Z/ { print $2 }; /^L/ { print $3 }" casual-timezone-zone-info-database)
    (split-string (buffer-string))))

(defun casual-timezone-map-local-to-timezone (ts remote-tz)
  "Map local TS to REMOTE-TZ."
  (let* ((parse-ts (string-split (org-read-date nil nil ts)))
         (datestamp (nth 0 parse-ts))
         (timestamp (nth 1 parse-ts))
         (local-tz (nth 1 (current-time-zone))))
    (format-time-string
     "%Y-%m-%d %H:%M:%S %Z"
     (date-to-time
      (concat datestamp "T" timestamp ":00" " " local-tz))
     remote-tz)))

(defun casual-timezone-local-time-to-remote (&optional datestr remote-tz)
  "Convert local date string DATESTR to remote timezone REMOTE-TZ.

The result is both copied to the `kill-ring' and messaged to the
mini-buffer.

If run interactively, the user will be prompted for a date string via
the `calendar' interface and the timezone via a completion interface.

The format of the timestamp is defined in the variable
`casual-timezone-convert-datestamp-format'."
  (interactive)
  (unless (not (eq system-type 'windows-nt))
    (error "Not available on Windows"))

  (let* ((datestr (or datestr
                        (org-read-date t nil nil nil nil
                                       (format-time-string
                                        "%H:%M"
                                        (current-time)))))
         (remote-tz (or remote-tz
                        (completing-read-default
                         "Remote Timezone: "
                         (casual-timezone-zone-info))))
         (parse-ts (string-split datestr))
         (datestamp (nth 0 parse-ts))
         (timestamp (or (nth 1 parse-ts) "00:00"))
         (ctz (current-time-zone))
         (local-tz (casual-timezone--zone-seconds-to-hours (nth 0 ctz)))
         (remote-time
          (format-time-string
           casual-timezone-convert-datestamp-format
           (encode-time (iso8601-parse
                         (concat datestamp "T" timestamp ":00" local-tz)))
           remote-tz))
         (remote-time-tz (concat remote-tz " " remote-time)))
    (kill-new remote-time-tz)
    (message remote-time-tz)
    remote-time-tz))

(defun casual-timezone--zone-seconds-to-hours (tz)
  "Convert TZ in seconds to ISO8601 timezone string."
  (let* ((abs-tz (abs tz))
         (hours (/ abs-tz 3600))
         (remain (round (* (/ (mod abs-tz 3600) 3600.0) 60)))
         (sign (if (< tz 0) "-" "+"))
         (result (format "%s%02d%02d" sign hours remain)))
    result))

(defun casual-timezone-remote-time-to-local (&optional datestr remote-tz)
  "Convert date string DATESTR in remote timezone REMOTE-TZ to local.

The result is both copied to the `kill-ring' and messaged to the
mini-buffer.

The format of the timestamp is defined in the variable
`casual-timezone-convert-datestamp-format'."
  (interactive)
  (unless (not (eq system-type 'windows-nt))
    (error "Not available on Windows"))

  (let* ((remote-tz (or remote-tz
                        (completing-read-default
                         "Remote Timezone: "
                         (casual-timezone-zone-info))))
         (datestr (or datestr
                        (org-read-date t nil nil nil nil
                                       (format-time-string
                                        "%H:%M"
                                        (current-time)))))
         (tzcode (casual-timezone-offset-8601
                  (nth 0 (current-time-zone nil remote-tz))))
         (parse-ts (string-split datestr))
         (datestamp (nth 0 parse-ts))
         (timestamp (or (nth 1 parse-ts) "00:00"))
         ;; (local-tz (nth 1 (current-time-zone)))
         (index-time
          (format-time-string
           casual-timezone-convert-datestamp-format
           (encode-time
            (iso8601-parse (concat datestamp "T" timestamp ":00" tzcode))))))
    (kill-new index-time)
    (message index-time)
    index-time))

(defun casual-timezone-offset-8601 (offset)
  "Compute OFFSET for ISO 8601 date."
  (let* ((hours (/ offset 3600))
         (fractional (% offset 3600))
         (minutes (if (zerop fractional)
                      fractional
                    (round (* (/ fractional 3600.0) 60))))
         (abs-hours (abs hours))
         (remote-tz (concat (format "%02d" abs-hours) (format "%02d" minutes))))

    (if (> hours 0)
        (concat "+" remote-tz)
      (concat "-" remote-tz))))

;; !!!: Unused code.
(defun casual-timezone--gen-hour-sequence (start duration)
  "Generate hour sequence given START, DURATION."
  (mapcar (lambda (x) (% x 24)) (number-sequence start (+ start duration))))

(defvar-keymap casual-timezone-planner-mode-map
  "C-o" #'casual-timezone-planner-tmenu
  "." #'casual-timezone-jump-to-relative-now
  "t" #'casual-timezone-planner-current-time
  "l" #'casual-timezone-planner-current-local
  "r" #'casual-timezone-planner-current-remote
  "f" #'casual-timezone-planner-forward-day
  "b" #'casual-timezone-planner-backward-day
  "p" #'previous-line
  "n" #'next-line
  "q" #'quit-window
  "j" #'next-line
  "k" #'previous-line
  "w" #'world-clock
  "z" #'casual-timezone-planner
  "T" #'casual-timezone-planner-current-point
  "c" #'calendar)

(define-derived-mode casual-timezone-planner-mode
  special-mode "Timezone Planner"
  "Major mode for Timezone Planner."
  (hl-line-mode t))

(defun casual-timezone-planner (remote-timezones datestamp)
  "Generate hours table between local and REMOTE-TIMEZONES on DATESTAMP.

REMOTE-TIMEZONES is a list of timezones to compare local time with. It
can be either be a `list' of strings or a comma or space separated
`string' type.

DATESTAMP is a string in YYYY-MM-DD format.

When called interactively, this command will prompt the user twice:
 1. to specify a remote timezone
 2. to specify a calendar day

Upon entering the above, a new buffer will be created (or
updated) comparing the hours between the two timezones.

The report datestamp format can be customized via the variable
`casual-timezone-datestamp-format'.

Working hours are annotated with a ☼.  The range of working hours can be
customized via the variable `casual-timezone-working-hours-range'."
  (interactive (list
                (completing-read-multiple "Remote Timezone(s): " (casual-timezone-zone-info))
                (org-read-date)))
  (when (stringp remote-timezones) (setq remote-timezones (split-string remote-timezones "[, ]+")))
  (unless (not (eq system-type 'windows-nt))
    (error "Not available on Windows"))

  (let* (;; (tzcode (casual-timezone-offset-8601 (nth 0 (current-time-zone nil remote-tz))))
         (local-tz (nth 1 (current-time-zone)))
         (start-time (date-to-time (concat datestamp " " "05:00")))
         (increments (seq-map (lambda (x) (seconds-to-time (* x 3600))) (number-sequence 0 25)))
         (tztimes (seq-map (lambda (x) (time-add start-time x)) increments))
         (local-times
          (seq-map
           (lambda (x) (format-time-string "%Y-%m-%d %H:%M:%S" x))
           tztimes))
         (remote-times
          (seq-map
           (lambda (remote-tz)
             (seq-map
              (lambda (x) (time-to-seconds (date-to-time (format-time-string
                                                          "%Y-%m-%dT%H:%M:%S"
                                                          (date-to-time (format-time-string (concat x " " local-tz)))
                                                          remote-tz))))
              local-times))
           remote-timezones))
         (tz-data (apply #'seq-mapn #'list tztimes remote-times))
         (tz-buffer-name (format "*%s - %s*" local-tz (string-join remote-timezones " - "))))

    (get-buffer-create tz-buffer-name)
    (switch-to-buffer (set-buffer tz-buffer-name))
    (casual-timezone-planner-mode)

    (let ((inhibit-read-only t))
        (erase-buffer)
        (make-vtable
         :columns (append
                   `((:name ,local-tz :width 30 :align left)) ;; !!! For some reason I can't pass in local-tz
                   (seq-map (lambda (remote-tz)
                              `(:name ,remote-tz :width 30 :align left))
                            remote-timezones))

         :objects tz-data

         :getter `(lambda (issue column table)
                    (nth column issue))

         :formatter `(lambda (value column table)
                       (casual-timezone--date-formatter value))

         :displayer `(lambda (fvalue index max-width table)
                       (propertize fvalue 'default 'bold)))
        (casual-timezone-jump-to-relative-now))))


(defun casual-timezone-planner-current-local ()
  "Copy local time on current line to `kill-ring'.

The format of the timestamp is defined in the variable
`casual-timezone-datestamp-format'."
  (interactive)
  (let ((result (casual-timezone-planner--format-current-index 0)))
    (kill-new result)
    (message result)))

(defun casual-timezone-planner-current-point ()
  "Copy time at current point to `kill-ring'.

The format of the timestamp is defined in the variable
`casual-timezone-datestamp-format'."
  (interactive)
  (let ((result (casual-timezone-planner--format-current-index
                 (vtable-current-column))))
    (kill-new result)
    (message result)))

(defun casual-timezone-planner-current-time ()
  "Copy times on current line to `kill-ring'.

The format of the timestamp is defined in the variable
`casual-timezone-datestamp-format'."
  (interactive)
  (let* ((result
          (string-join
           (seq-map-indexed
            (lambda (_ i)
              (casual-timezone-planner--format-current-index i))
            (vtable-current-object))
           "; ")))
    (kill-new result)
    (message result)))

(defun casual-timezone-planner--format-current-index (arg)
  "Copy element with ARG index in current vtable object into `kill-ring'."
  (unless (vtable-current-table) (error "No planner table"))

  (let* ((name (vtable-column (vtable-current-table) arg))
         (obj (vtable-current-object))
         (index-time (nth arg obj))
         (result (format
                  "%s - %s"
                  name
                  (format-time-string
                   casual-timezone-datestamp-format
                   index-time))))
    result))

(defun casual-timezone-jump-to-relative-now ()
  "Jump to current relative hour in timezone planner view.

This command is used for the planner generated by
`casual-timezone-planner'."
  (interactive)
  (unless (vtable-current-table) (error "No planner table"))

  (let ((current-table (vtable-current-table)))
    (if current-table
        (let* ((table-data (vtable-objects current-table))
               (idxObj (car table-data))
               (now (casual-timezone--relative-now (car idxObj))))

          (while table-data
            (let* ((obj (car table-data))
                   (index-time (car obj)))
              (if (< index-time now)
                  (setq idxObj obj)))
            (setq table-data (cdr table-data)))
          (vtable-goto-object idxObj)))))

(defun casual-timezone--relative-now (plan-time)
  "Adjust current time (hour:minutes) to date relative to planner date PLAN-TIME.

This command is used for the planner generated by
`casual-timezone-planner'."
  (let* ((datestamp-buf (format-time-string "%Y-%m-%d" plan-time))
         (datestamp (date-to-time datestamp-buf))
         (now (current-time))
         (hours (string-to-number (format-time-string "%H" now)))
         (minutes (string-to-number (format-time-string "%M" now)))
         (offset (+ (* (* hours 60) 60) (* minutes 60)))
         (datestamp-adj (time-add datestamp offset)))
    datestamp-adj))


(defun casual-timezone--date-formatter (timestamp)
  "Datestamp formatter given TIMESTAMP.

This formats the output result using the customizable variables
`casual-timezone-datestamp-format' and `casual-timezone-working-hours-range'."
  (let ((hour (string-to-number (format-time-string "%H" timestamp)))
        (datestamp (format-time-string casual-timezone-datestamp-format timestamp)))

    (if (and (>= hour (map-elt casual-timezone-working-hours-range :start))
             (<= hour (map-elt casual-timezone-working-hours-range :stop)))
        (propertize
         (concat datestamp " " casual-timezone-working-hour-glyph)
         'face
         'casual-timezone-planner-working-highlight)
      datestamp)))

(defun casual-timezone-planner-forward-day ()
  "Move forward one day in timezone planner.

Note: This command relies on `vtable-update-object' which breaks if the
window width has changed."
  (interactive)
  (unless (vtable-current-table) (error "No planner table"))
  (casual-timezone--planner-adjust-day nil))

(defun casual-timezone-planner-backward-day ()
  "Move backward one day in timezone planner.

Note: This command relies on `vtable-update-object' which breaks if the
window width has changed."
  (interactive)
  (unless (vtable-current-table) (error "No planner table"))
  (casual-timezone--planner-adjust-day t))

(defun casual-timezone--planner-adjust-day (backward)
  "If BACKWARD is non-nil, adjust timezone planner -24 hours, otherwise ahead."
  (let* ((table (vtable-current-table))
         (objects (oref table objects))
         (posix-day 86400)
         (time-adjust (if backward
                          (* -1 posix-day)
                        posix-day)))
    (mapc (lambda (obj)
            (let ((adjusted-obj
                   (seq-map (lambda (x) (+ x time-adjust)) obj)))
              (vtable-update-object table adjusted-obj obj)))
          objects)
    (casual-timezone-jump-to-relative-now)))

;; Transients
(transient-define-prefix casual-timezone-planner-tmenu ()
  "Main menu for Casual Timezone."

  ["Casual Timezone"
   ["Navigation"
    :pad-keys t
    ("." "Current Hour" casual-timezone-jump-to-relative-now
     :description (lambda () (casual-timezone-unicode-get :current))
     :transient t)
    ("p" "Previous" previous-line
     :description (lambda () (casual-timezone-unicode-get :previous))
     :transient t)
    ("n" "Next" next-line
     :description (lambda () (casual-timezone-unicode-get :next))
     :transient t)
    ("TAB" "Right" vtable-next-column
     :description (lambda () (casual-timezone-unicode-get :right))
     :transient t)
    ("S-TAB" "Left" vtable-previous-column
     :description (lambda () (casual-timezone-unicode-get :left))
     :transient t)]

   ["Day"
    ("f" "Forward" casual-timezone-planner-forward-day
     :description (lambda () (casual-timezone-unicode-get :forward))
     :transient t)
    ("b" "Backward" casual-timezone-planner-backward-day
     :description (lambda () (casual-timezone-unicode-get :backward))
     :transient t)]

   ["Copy Time"
    ("t" "Times" casual-timezone-planner-current-time)
    ("l" "Local" casual-timezone-planner-current-local)
    ;; ("r" "Remote" casual-timezone-planner-current-remote)
    ("T" "Point" casual-timezone-planner-current-point)]

   ["Misc"
    ("z" "Planner…" casual-timezone-planner)
    ("w" "World Clock" world-clock)
    ("c" "Calendar" calendar)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("," "Settings" casual-timezone-settings-tmenu)
   ("q" "Quit" quit-window)
   (casual-lib-quit-all)])

(provide 'casual-timezone-utils)
;;; casual-timezone-utils.el ends here
