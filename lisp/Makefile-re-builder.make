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

EMACS_MAC_APP_HOME=/Applications/MacPorts/EmacsMac.app
EMACS_MAC_APP_SH=$(EMACS_MAC_APP_HOME)/Contents/MacOS/Emacs.sh

ifneq ("$(wildcard $(EMACS_MAC_APP_SH))","")
  EXEC_NAME=$(EMACS_MAC_APP_SH)
else
  EXEC_NAME=emacs
endif

PACKAGE_NAME=casual-re-builder
ELISP_INCLUDES=casual-re-builder-version.el	\
casual-re-builder-utils.el				\
casual-re-builder-settings.el
ELISP_PACKAGES=
ELISP_TEST_INCLUDES=casual-re-builder-test-utils.el
CASUAL_BASE_DIR=$(HOME)/Projects/elisp
CASUAL_LIB_DIR=$(CASUAL_BASE_DIR)/casual-lib
CASUAL_LIB_LISP_DIR=$(CASUAL_LIB_DIR)/lisp
CASUAL_LIB_TEST_INCLUDES=$(CASUAL_LIB_DIR)/tests/casual-lib-test-utils.el
EMACS_ELPA_DIR=$(HOME)/.config/emacs/elpa
PACKAGE_PATHS=					\
-L $(EMACS_ELPA_DIR)/compat-30.0.0.0		\
-L $(EMACS_ELPA_DIR)/seq-2.24			\
-L $(EMACS_ELPA_DIR)/transient-current		\
-L $(CASUAL_LIB_LISP_DIR)

.PHONY: tests compile regression

.SUFFIXES: .el .elc .elt

.el.elc :
	$(EXEC_NAME) -Q --batch $(patsubst %, -l %, $(ELISP_INCLUDES)) \
-f batch-byte-compile $<

.el.elt :
	$(EXEC_NAME) -Q --batch				\
$(PACKAGE_PATHS)					\
$(patsubst %, -l %, $(ELISP_INCLUDES))			\
-l $<							\
-l $(CASUAL_LIB_TEST_INCLUDES)				\
-l $(patsubst %, ../tests/%, $(ELISP_TEST_INCLUDES))	\
-l $(patsubst %, ../tests/test-%, $<)			\
-f ert-run-tests-batch-and-exit

tests: $(ELISP_PACKAGES:.el=.elt) $(ELISP_INCLUDES:.el=.elt) $(PACKAGE_NAME).elt

compile: $(ELISP_PACKAGES:.el=.elc) $(ELISP_INCLUDES:.el=.elc) $(PACKAGE_NAME).elc

$(PACKAGE_NAME).elc: $(PACKAGE_NAME).el
	$(EXEC_NAME) -Q --batch $(patsubst %, -l %, $(ELISP_INCLUDES))	\
$(patsubst %, -l %, $(ELISP_PACKAGES))					\
$(PACKAGE_PATHS)							\
-f batch-byte-compile $<

$(PACKAGE_NAME).elt: $(PACKAGE_NAME).el
	$(EXEC_NAME) -Q --batch			\
$(PACKAGE_PATHS)				\
$(patsubst %, -l %, $(ELISP_INCLUDES))		\
$(patsubst %, -l %, $(ELISP_PACKAGES))		\
-l $<						\
-l $(CASUAL_LIB_TEST_INCLUDES)			\
-l ../tests/$(ELISP_TEST_INCLUDES)		\
-l $(patsubst %, ../tests/test-%, $<)		\
-f ert-run-tests-batch-and-exit

regression: clean compile tests

clean:
	rm -f *.elc
