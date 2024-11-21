##
# Copyright 2024 Charles Y. Choi
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
bookmarks-tests					\
calc-tests					\
calendar-tests					\
dired-tests					\
editkit-tests					\
ibuffer-tests					\
info-tests					\
isearch-tests					\
re-builder-tests

SRC_DIR=.

tests: lib-tests				\
agenda-tests					\
bookmarks-tests					\
calc-tests					\
calendar-tests					\
dired-tests					\
editkit-tests					\
ibuffer-tests					\
info-tests					\
isearch-tests					\
re-builder-tests

lib-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-lib.make tests

agenda-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-agenda.make tests

bookmarks-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-bookmarks.make tests

calc-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-calc.make tests

calendar-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-calendar.make tests

dired-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-dired.make tests

editkit-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-editkit.make tests

ibuffer-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-ibuffer.make tests

info-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-info.make tests

isearch-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-isearch.make tests

re-builder-tests:
	$(MAKE) -C $(SRC_DIR) -f Makefile-re-builder.make tests
