##
# Copyright (C) 2024-2025 Charles Y. Choi
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

.PHONY: tests					\
lib-tests					\
agenda-tests					\
bibtex-tests					\
bookmarks-tests					\
calc-tests					\
calendar-tests					\
compile-tests					\
dired-tests					\
editkit-tests					\
elisp-tests					\
eshell-tests					\
help-tests					\
ibuffer-tests					\
info-tests					\
isearch-tests					\
make-mode-tests					\
man-tests					\
re-builder-tests				\
timezone-tests

SRC_DIR=../lisp

tests: lib-tests				\
agenda-tests					\
bibtex-tests					\
bookmarks-tests					\
calc-tests					\
calendar-tests					\
compile-tests					\
dired-tests					\
editkit-tests					\
elisp-tests					\
eshell-tests					\
help-tests					\
ibuffer-tests					\
info-tests					\
isearch-tests					\
make-mode-tests					\
man-tests					\
re-builder-tests				\
timezone-tests

lib-tests:
	$(MAKE) -C $(SRC_DIR) $@

agenda-tests:
	$(MAKE) -C $(SRC_DIR) $@

bibtex-tests:
	$(MAKE) -C $(SRC_DIR) $@

bookmarks-tests:
	$(MAKE) -C $(SRC_DIR) $@

calc-tests:
	$(MAKE) -C $(SRC_DIR) $@

calendar-tests:
	$(MAKE) -C $(SRC_DIR) $@

compile-tests:
	$(MAKE) -C $(SRC_DIR) $@

dired-tests:
	$(MAKE) -C $(SRC_DIR) $@

editkit-tests:
	$(MAKE) -C $(SRC_DIR) $@

elisp-tests:
	$(MAKE) -C $(SRC_DIR) $@

eshell-tests:
	$(MAKE) -C $(SRC_DIR) $@

help-tests:
	$(MAKE) -C $(SRC_DIR) $@

ibuffer-tests:
	$(MAKE) -C $(SRC_DIR) $@

info-tests:
	$(MAKE) -C $(SRC_DIR) $@

isearch-tests:
	$(MAKE) -C $(SRC_DIR) $@

make-mode-tests:
	$(MAKE) -C $(SRC_DIR) $@

man-tests:
	$(MAKE) -C $(SRC_DIR) $@

re-builder-tests:
	$(MAKE) -C $(SRC_DIR) $@

timezone-tests:
	$(MAKE) -C $(SRC_DIR) $@
