##
# Copyright (C) 2024-2026 Charles Y. Choi
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
SRC_DIR=.

.PHONY: tests
tests: lib-tests				\
agenda-tests					\
bibtex-tests					\
bookmarks-tests					\
calc-tests					\
calendar-tests					\
compile-tests					\
css-tests					\
csv-tests					\
dired-tests					\
ediff-tests					\
editkit-tests					\
elisp-tests					\
eshell-tests					\
eww-tests					\
help-tests					\
html-tests					\
ibuffer-tests					\
info-tests					\
isearch-tests					\
make-mode-tests					\
man-tests					\
org-tests					\
re-builder-tests				\
timezone-tests

.PHONY: lib-tests
lib-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-lib.make tests

.PHONY: agenda-tests
agenda-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-agenda.make tests

.PHONY: bibtex-tests
bibtex-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-bibtex.make tests

.PHONY: bookmarks-tests
bookmarks-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-bookmarks.make tests

.PHONY: calc-tests
calc-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-calc.make tests

.PHONY: calendar-tests
calendar-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-calendar.make tests

.PHONY: compile-tests
compile-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-compile.make tests

.PHONY: css-tests
css-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-css.make tests

.PHONY: csv-tests
csv-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-csv.make tests

.PHONY: dired-tests
dired-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-dired.make tests

.PHONY: ediff-tests
ediff-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-ediff.make tests

.PHONY: editkit-tests
editkit-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-editkit.make tests

.PHONY: elisp-tests
elisp-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-elisp.make tests

.PHONY: eshell-tests
eshell-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-eshell.make tests

.PHONY: eww-tests
eww-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-eww.make tests

.PHONY: help-tests
help-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-help.make tests

.PHONY: html-tests
html-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-html.make tests

.PHONY: image-tests
image-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-image.make tests

.PHONY: ibuffer-tests
ibuffer-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-ibuffer.make tests

.PHONY: info-tests
info-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-info.make tests

.PHONY: isearch-tests
isearch-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-isearch.make tests

.PHONY: make-mode-tests
make-mode-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-make-mode.make tests

.PHONY: man-tests
man-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-man.make tests

.PHONY: org-tests
org-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-org.make tests

.PHONY: re-builder-tests
re-builder-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-re-builder.make tests

.PHONY: timezone-tests
timezone-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-timezone.make tests
