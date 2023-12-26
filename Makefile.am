GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

TESTS = tests/context.scm tests/state.scm
AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"
TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER = $(top_builddir)/pre-inst-env $(GUILE) --no-auto-compile \
   $(top_srcdir)/build-aux/test-driver.scm
CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(TESTS)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
SOURCES = xkbcommon/config.scm \
		xkbcommon/xkbregistry.scm \
		xkbcommon/xkbcommon.scm \
		xkbcommon/keysyms.scm
