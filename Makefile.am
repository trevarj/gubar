bin_SCRIPTS = scripts/gubar

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_dist_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = gubar/blocks/xkb-layout.scm \
          gubar/blocks/volume-pipewire.scm \
          gubar/blocks/network-manager-wifi.scm \
          gubar/blocks/label.scm \
          gubar/blocks/date-time.scm \
          gubar/blocks/battery.scm \
          gubar/hconfig.scm \
          gubar/main.scm \
          gubar/swaybar-protocol.scm \
          gubar/gublock.scm

TESTS = 

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = 

EXTRA_DIST = COPYING \
             README.md \
             NEWS \
             AUTHORS \
             ChangeLog \
             guix.scm \
             .gitignore \
             hall.scm \
             build-aux/texinfo.tex \
             build-aux/missing \
             build-aux/install-sh \
             build-aux/test-driver.scm \
             configure.ac \
             Makefile.am \
             pre-inst-env.in \
             $(TESTS)

ACLOCAL_AMFLAGS = -I m4

AM_DISTCHECK_DVI_TARGET = info # Disable DVI as part of distcheck

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(BUILT_SOURCES)				\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
