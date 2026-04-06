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

MODULES=					\
lib						\
agenda						\
bibtex						\
bookmarks					\
calc						\
calendar					\
compile						\
css						\
csv						\
dired						\
ediff						\
editkit						\
elisp						\
eshell						\
eww						\
help						\
html						\
ibuffer						\
info						\
isearch						\
ispell						\
make-mode					\
man						\
org						\
re-builder					\
timezone

TEST_MODULES := $(MODULES:%=%-tests)
PKGLINT_MODULES := $(MODULES:%=%-pkglint)
COMPILE_MODULES := $(MODULES:%=%-compile)

.PHONY: tests $(TEST_MODULES)
tests: $(TEST_MODULES)

.PHONY: package-lint $(PKGLINT_MODULES) pkglint
package-lint: $(PKGLINT_MODULES)
pkglint: package-lint

.PHONY: compile $(COMPILE_MODULES)
compile: $(COMPILE_MODULES)

.PHONY: clean
clean:
	rm -f *.elc

.PHONY: regression
regression: clean tests compile package-lint

$(TEST_MODULES):
	$(MAKE) -C $(SRC_DIR) -f Makefile-$(@:%-tests=%).make tests

$(PKGLINT_MODULES):
	$(MAKE) -C $(SRC_DIR) -f Makefile-$(@:%-pkglint=%).make package-lint

$(COMPILE_MODULES):
	$(MAKE) -C $(SRC_DIR) -f Makefile-$(@:%-compile=%).make compile
