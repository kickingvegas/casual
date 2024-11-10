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
include Makefile--defines.make

PACKAGE_NAME=casual-calc

ELISP_INCLUDES =				\
casual-calc--calc.el				\
casual-calc-utils.el				\
casual-calc-algebra.el				\
casual-calc-predicates.el			\
casual-calc-labels.el				\
casual-calc-fileio.el				\
casual-calc-radix.el				\
casual-calc-angle-measure.el			\
casual-calc-stack.el				\
casual-calc-variables.el			\
casual-calc-graphics.el

ELISP_PACKAGES=					\
casual-calc-binary.el				\
casual-calc-complex.el				\
casual-calc-conversion.el			\
casual-calc-logarithmic.el			\
casual-calc-random.el				\
casual-calc-rounding.el				\
casual-calc-settings.el				\
casual-calc-time.el				\
casual-calc-trail.el				\
casual-calc-trigonometric.el			\
casual-calc-units.el				\
casual-calc-vector.el				\
casual-calc-financial.el			\
casual-calc-symbolic.el

ELISP_TEST_INCLUDES=casual-calc-test-utils.el

PACKAGE_PATHS=-L $(CASUAL_LIB_LISP_DIR)

include Makefile--rules.make
