dnl -*- Autoconf -*-

AC_INIT(gubar, 0.1)
AC_SUBST(HVERSION, "\"0.1\"")
AC_SUBST(AUTHOR, "\"Trevor Arjeski\"")
AC_SUBST(COPYRIGHT, "'(2025)")
AC_SUBST(LICENSE, gpl3+)
AC_CONFIG_SRCDIR(scripts)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([scripts/gubar],[chmod +x scripts/gubar])
dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

dnl Hall auto-generated guile-module dependencies
GUILE_MODULE_REQUIRED([fibers])
GUILE_MODULE_REQUIRED([json])

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_OUTPUT
