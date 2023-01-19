#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-xkbcommon], [0.0.1], [873216071@qq.com])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([xkbcommon/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

dnl AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"

PKG_CHECK_MODULES([GUILE], [guile-3.0])
PKG_CHECK_MODULES([XKBCOMMON],[xkbcommon])
PKG_CHECK_MODULES([XKBREGISTRY],[xkbregistry])

PKG_CHECK_VAR([XKBCOMMON_LIBDIR],[xkbcommon],[libdir])
PKG_CHECK_VAR([XKBREGISTRY_LIBDIR],[xkbregistry],[libdir])
AC_SUBST([XKBCOMMON_LIBDIR])
AC_SUBST([XKBREGISTRY_LIBDIR])
GUILE_MODULE_REQUIRED([bytestructures guile])
GUILE_MODULE_REQUIRED([bytestructure-class])

AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

# Checks for programs.

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
