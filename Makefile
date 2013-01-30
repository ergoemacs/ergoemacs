# How to release a new ErgoEmacs distribution?
#
# Just run "make update-version VERSION=x.x.x"
# And then "make compile"

CURRENT_VERSION = $(shell cat ergoemacs/init_version.el \
			| grep "defconst ergoemacs-version " \
			| sed -e "s/^.*ergoemacs-version \"*\([0-9.]*\).*/\1/")

RC_VERSION = $(shell echo $(VERSION) | sed -e "s/\\./,/g" -e "s/$$/,0/")
RC_VERSION2 = $(shell echo $(RC_VERSION) | sed -e "s/,/, /g")

ifndef EMACS
EMACS = emacs
endif

RM = rm
ELC_FILES = $(wildcard *.elc) $(wildcard */*.elc) $(wildcard */*/*.elc)

PACKAGE_AUTOCOMPLETE = auto-complete
PACKAGE_DICTIONARY = dictionary-1.8.7
PACKAGE_EXPANDREGION = expand-region
PACKAGE_POPUP = popup-20121020.1203
PACKAGE_HUNSPELL = rw-hunspell
PACKAGE_YASNIPPET = yasnippet-0.6.1c

all:
	@echo "Usage:"
	@echo "  make compile"
	@echo "    Byte-compiles all .el files with the specified emacs in EMACS variable."
	@echo "    E.g. You can use it as:"
	@echo "      make compile EMACS=../emacs-24.2/bin/emacs.exe"
	@echo
	@echo "  make clean"
	@echo "    Removes all .elc files."
	@echo
	@echo "  make show-version"
	@echo "    Shows the current ErgoEmacs version."
	@echo
	@echo "  make update-version VERSION=x.x.x"
	@echo "    Changes the ErgoEmacs version from all files."
	@echo

# Byte-compiles all .el files
compile:
	-$(RM) -f $(ELC_FILES)
	$(EMACS) -L packages \
		 -L packages/$(PACKAGE_AUTOCOMPLETE) \
		 -L packages/$(PACKAGE_EXPANDREGION) \
		 -L packages/$(PACKAGE_POPUP) \
		 -L packages/$(PACKAGE_YASNIPPET) \
		 -batch -f batch-byte-compile ergoemacs/*.el
	-$(EMACS) -batch -f batch-byte-compile ergoemacs/ergoemacs-keybindings/*.el
	-$(EMACS) -batch -f batch-byte-compile packages/*.el
	$(EMACS) -L packages/$(PACKAGE_AUTOCOMPLETE) \
		 -L packages/$(PACKAGE_POPUP) \
		 -batch -f batch-byte-compile packages/$(PACKAGE_AUTOCOMPLETE)/*.el
	$(EMACS) -L packages/$(PACKAGE_DICTIONARY) -batch -f batch-byte-compile packages/$(PACKAGE_DICTIONARY)/*.el
	$(EMACS) -L packages/$(PACKAGE_EXPANDREGION) -batch -f batch-byte-compile packages/$(PACKAGE_EXPANDREGION)/*.el
	$(EMACS) -L packages/$(PACKAGE_POPUP) -batch -f batch-byte-compile packages/$(PACKAGE_POPUP)/*.el
	$(EMACS) -L packages/$(PACKAGE_HUNSPELL) -batch -f batch-byte-compile packages/$(PACKAGE_HUNSPELL)/*.el
	$(EMACS) -L packages/$(PACKAGE_YASNIPPET) -batch -f batch-byte-compile packages/$(PACKAGE_YASNIPPET)/*.el
	$(EMACS) -batch -f batch-byte-compile site-lisp/*.el

# Removes all .elc files
clean:
	-$(RM) -f $(ELC_FILES)

# Displays the current ErgoEmacs version
show-version:
	@echo $(CURRENT_VERSION)

# Changes the ErgoEmacs version
update-version:
	cat ergoemacs/init_version.el \
		| sed -e "s/defconst ergoemacs-version \"[0-9.]*\"/defconst ergoemacs-version \"$(VERSION)\"/" \
		> tmp
	mv tmp ergoemacs/init_version.el
	cat win32-setup/ErgoEmacs.iss \
		| sed -e "s/\(#define AppVersion *\)\"[0-9.]*\"/\1\"$(VERSION)\"/" \
		> tmp
	mv tmp win32-setup/ErgoEmacs.iss
	cat win32-setup/ErgoEmacs.rc \
		| sed -e "s/ FILEVERSION [0-9, ]*/ FILEVERSION $(RC_VERSION)/" \
		      -e "s/ PRODUCTVERSION [0-9, ]*/ PRODUCTVERSION $(RC_VERSION)/" \
		      -e "s/VALUE \"FileVersion\",.*/VALUE \"FileVersion\", \"$(RC_VERSION2)\\\\0\"/" \
		      -e "s/VALUE \"ProductVersion\",.*/VALUE \"ProductVersion\", \"$(RC_VERSION2)\\\\0\"/" \
		> tmp
	mv tmp win32-setup/ErgoEmacs.rc
