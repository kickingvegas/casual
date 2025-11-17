##
# Copyright 2025 Charles Y. Choi
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
#

TIMESTAMP := $(shell /bin/date "+%Y%m%d_%H%M%S")
INSTALL_DIR=..

MODULE ?= foo
CASUAL_MODULE=casual-$(MODULE)
CASUAL_TEMPLATE=casual-MODULE

.PHONY: source-files test-files doc-files

all: source-files test-files doc-files

source-files:						\
$(INSTALL_DIR)/lisp/$(CASUAL_MODULE).el			\
$(INSTALL_DIR)/lisp/$(CASUAL_MODULE)-utils.el		\
$(INSTALL_DIR)/lisp/$(CASUAL_MODULE)-settings.el	\
$(INSTALL_DIR)/lisp/Makefile-$(MODULE).make


test-files:						\
$(INSTALL_DIR)/tests/$(CASUAL_MODULE)-test-utils.el	\
$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE).el		\
$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE)-utils.el	\
$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE)-settings.el

doc-files: $(INSTALL_DIR)/docs/$(MODULE).org

# Source Files
$(INSTALL_DIR)/lisp/$(CASUAL_MODULE).el:
	MODULE=$(MODULE) envsubst < lisp/$(CASUAL_TEMPLATE).el > $@

$(INSTALL_DIR)/lisp/$(CASUAL_MODULE)-utils.el:
	MODULE=$(MODULE) envsubst < lisp/$(CASUAL_TEMPLATE)-utils.el > $@

$(INSTALL_DIR)/lisp/$(CASUAL_MODULE)-settings.el:
	MODULE=$(MODULE) envsubst < lisp/$(CASUAL_TEMPLATE)-settings.el > $@

$(INSTALL_DIR)/lisp/Makefile-$(MODULE).make:
	MODULE=$(MODULE) envsubst < lisp/Makefile-MODULE.make > $@

# Test Files
$(INSTALL_DIR)/tests/$(CASUAL_MODULE)-test-utils.el:
	MODULE=$(MODULE) envsubst < tests/$(CASUAL_TEMPLATE)-test-utils.el > $@

$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE).el:
	MODULE=$(MODULE) envsubst < tests/test-$(CASUAL_TEMPLATE).el > $@

$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE)-utils.el:
	MODULE=$(MODULE) envsubst < tests/test-$(CASUAL_TEMPLATE)-utils.el > $@

$(INSTALL_DIR)/tests/test-$(CASUAL_MODULE)-settings.el:
	MODULE=$(MODULE) envsubst < tests/test-$(CASUAL_TEMPLATE)-settings.el > $@

# Doc Files
$(INSTALL_DIR)/docs/$(MODULE).org:
	MODULE=$(MODULE) envsubst < docs/MODULE.org > $@
