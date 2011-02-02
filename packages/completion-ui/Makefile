
# Copyright (C) 2009 Toby Cubitt

# Author: Toby Cubitt <toby-predictive@dr-qubit.org>
# Version: 0.1
# URL: http://www.dr-qubit.org/emacs.php

# This file is part of the Emacs Completion-UI Completion package.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.


EMACS = emacs

# list of core elisp files
el_files := $(shell ls *.el | sed 's:\.el$$:\.elc:' )

# implicit rule for byte-compiling elisp files
%.elc: %.el
	$(EMACS) --batch -L ./ -L ../../lisp -f batch-byte-compile $<

all: $(el_files)

