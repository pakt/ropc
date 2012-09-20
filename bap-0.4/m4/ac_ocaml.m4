# OCaml macros for autoconf
#
# Guillaume Rousse <Guillaume.Rou...@inria.fr>
# inspired by previous work from:
# Georges Mariano
# Jean-Christophe Filliâtre
# Olivier Andrieu
# Grigory Batalov

# AC_PROG_OCAML([MINIMUM VERSION])
# --------------------------------
#  check OCaml base system, and set the following variables:
#  OCAMLC       OCaml compiler
#  OCAMLC_OPT   OCaml optimized compiler
#  OCAMLOPT     OCaml native compiler
#  OCAMLOPT_OPT OCaml optimized native compiler
#  OCAMLDEP     OCaml dependency generator
#  OCAMLDEP_OPT OCaml optimized dependency generator
#  OCAMLMKTOP   OCaml toplevel system builder
#  OCAMLMKLIB   OCaml library builder
#  OCAMLDOC     OCaml documentation generator
#  OCAMLDOC_OPT OCaml optimized documentation generator
#  OCAMLLIB     OCaml library path
#  OCAMLVERSION OCaml version number
#  OCAMLLEX     OCaml lexer
#  OCAMLLEX_OPT OCaml optimized lexer
#  OCAMLYACC    OCaml parser
#  Fails if ocaml compiler is not found.

#  Modified by Sojeong Hong
AC_DEFUN([AC_PROG_OCAML], [

    # allow the user to disable the use of optimized versions
    AC_ARG_ENABLE(
        [opt],
        AC_HELP_STRING(
            [--enable-opt],
            [use optimized versions of ocaml tools (default)]
        ),
        [case "$enableval" in
            yes) ac_ocaml_enable_opt=$enableval;;
            no)  ac_ocaml_enable_opt=$enableval;;
            *)   AC_MSG_ERROR([bad value $enableval for --enable-opt]);;
        esac],
        [ac_ocaml_enable_opt="yes"]
    )

    # Checking for OCaml compiler
    _AC_OCAML_PATH_PROG_FATAL(OCAMLC, ocamlc)

    # Checking for OCaml version
    AC_MSG_CHECKING([for OCaml version])
    OCAMLVERSION=`$OCAMLC -version`
    AC_MSG_RESULT([$OCAMLVERSION])

    if test -n ["$1"]; then
        ac_ocaml_min_version=["$1"];
        # Checking for OCaml minimum version
        AC_MSG_CHECKING([whether OCaml version >= $ac_ocaml_min_version])
        ac_ocaml_min_major_version=`echo $ac_ocaml_min_version | cut -d. -f1`
        ac_ocaml_min_minor_version=`echo $ac_ocaml_min_version | cut -d. -f2`
        ac_ocaml_min_micro_version=`echo $ac_ocaml_min_version | cut -d. -f3`
        ac_ocaml_major_version=`echo $OCAMLVERSION | cut -d. -f1`
        ac_ocaml_minor_version=`echo $OCAMLVERSION | cut -d. -f2`
        ac_ocaml_micro_version=`echo $OCAMLVERSION | cut -d. -f3`

        if expr                                                        \
        \(                                                             \
            $ac_ocaml_major_version \> $ac_ocaml_min_major_version     \
        \) \|                                                          \
        \(                                                             \
            $ac_ocaml_major_version \= $ac_ocaml_min_major_version \&  \
            $ac_ocaml_minor_version \> $ac_ocaml_min_minor_version     \
        \) \|                                                          \
        \(                                                             \
            $ac_ocaml_major_version \=  $ac_ocaml_min_major_version \& \
            $ac_ocaml_minor_version \=  $ac_ocaml_min_minor_version \& \
            $ac_ocaml_micro_version \>= $ac_ocaml_min_micro_version    \
        \) > /dev/null; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([OCaml version unsufficient])
        fi
    fi

    # Checking for OCaml library path
    AC_MSG_CHECKING([for OCaml library path])
    OCAMLLIB=`$OCAMLC -where`
    AC_MSG_RESULT([$OCAMLLIB])

    if test "$ac_ocaml_enable_opt" = "yes"; then
        # Checking for ocamlc.opt
        _AC_OCAML_PATH_PROG_NONFATAL(OCAMLC_OPT, ocamlc.opt)
        if test -n "$OCAMLC_OPT"; then
            _AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLC_OPT, ocamlc.opt)
        fi
        if test -n "$OCAMLC_OPT"; then
            OCAMLC=$OCAMLC_OPT
        fi
    fi

    # Checking for OCaml native compiler
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLOPT, ocamlopt, [Cannot find ocamlopt; bytecode compilation only])
    if test -n "$OCAMLOPT"; then
        _AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLOPT, ocamlopt)
    fi
    if test -n "$OCAMLOPT"; then
        touch conftest.c

        AC_MSG_CHECKING([if OCaml C compiler works])
        if $OCAMLC conftest.c >/dev/null 2>&1; then
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
            AC_MSG_WARN([bytecode compilation only])
            unset OCAMLOPT
        fi

        rm -f conftest.c
    fi

    if test "$ac_ocaml_enable_opt" = "yes"; then
        # Checking for ocamlopt.opt
        _AC_OCAML_PATH_PROG_NONFATAL(OCAMLOPT_OPT, ocamlopt.opt)
        if test -n "$OCAMLOPT_OPT"; then
            _AC_OCAML_CHECK_VERSION_NONFATAL(OCAMLOPT_OPT, ocamlopt.opt)
        fi
        if test -n "$OCAMLOPT_OPT"; then
            OCAMLOPT=$OCAMLOPT_OPT
        fi
    fi

    # Checking for ocamldep
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLDEP, ocamldep)

    if test "$ac_ocaml_enable_opt" = "yes"; then
        # Checking for ocamldep.opt
        _AC_OCAML_PATH_PROG_NONFATAL(OCAMLDEP_OPT, ocamldep.opt)
        if test -n "$OCAMLDEP_OPT"; then
            OCAMLDEP=$OCAMLDEP_OPT
        fi
    fi

    # Checking for ocamlmktop
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLMKTOP, ocamlmktop)

    # Checking for ocamlmktop
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLMKLIB, ocamlmklib)

    # Checking for ocamldoc
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLDOC, ocamldoc)

    if test "$ac_ocaml_enable_opt" = "yes"; then
        # Checking for ocamldoc.opt
        _AC_OCAML_PATH_PROG_NONFATAL(OCAMLDOC_OPT, ocamldoc.opt)
        if test -n "$OCAMLDOC_OPT"; then
            OCAMLDOC=$OCAMLDOC_OPT
        fi
    fi

    # Checking for ocamllex
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLLEX, ocamllex)

    if test "$ac_ocaml_enable_opt" = "yes"; then
        # Checking for ocamllex.opt
        _AC_OCAML_PATH_PROG_NONFATAL(OCAMLLEX_OPT, ocamllex.opt)
        if test -n "$OCAMLLEX_OPT"; then
            OCAMLLEX=$OCAMLLEX_OPT
        fi
    fi

    # Checking for ocamlyacc
    _AC_OCAML_PATH_PROG_NONFATAL(OCAMLYACC, ocamlyacc)

]) # AC_PROG_OCAML

# AC_PROG_CAMLP4
# --------------
# Check CamlP4 and set the following variables:
#   CAMLP4      camlp4
#   CAMLP4O     camlp4o
#   CAMLP4R     camlp4r
#   CAMLP4LIB   parser library path
#  Fails if camlp4 is not found
AC_DEFUN([AC_PROG_CAMLP4], [
    AC_REQUIRE([AC_PROG_OCAML])

    # Checking for camlp4
    _AC_OCAML_PATH_PROG_FATAL(CAMLP4, camlp4)
    _AC_OCAML_CHECK_VERSION_FATAL(CAMLP4, camlp4)

    # Checking for Camlp4o
    _AC_OCAML_PATH_PROG_NONFATAL(CAMLP4O, camlp4o)

    # Checking for Camlp4r
    _AC_OCAML_PATH_PROG_NONFATAL(CAMLP4R, camlp4r)

    # Searching for parser library path
    AC_MSG_CHECKING([for CamlP4 library path])
    CAMLP4LIB=`$CAMLP4 -where`
    AC_MSG_RESULT([$CAMLP4LIB])

]) # AC_PROG_CAMLP4

# added by Sojeong Hong
# checking if ocamlfind is installed
AC_DEFUN([AC_PROG_OCAMLFIND], [
    AC_REQUIRE([AC_PROG_OCAML])
    _AC_OCAML_PATH_PROG_FATAL(OCAMLFIND, ocamlfind)
]) 

#checking if camlidl is installed
AC_DEFUN([AC_PROG_CAMLIDL], [
    AC_REQUIRE([AC_PROG_OCAML])
    _AC_OCAML_PATH_PROG_FATAL(CAMLIDL, camlidl)
])


# _AC_OCAML_PATH_PROG_FATAL(VARIABLE, PROGRAM, [MESSAGE])
# -------------------------------------------------------
# wraps AC_PATH_PROG, issuing an error if PROGRAM
# is not found, otherwise affects its path to VARIABLE
AC_DEFUN([_AC_OCAML_PATH_PROG_FATAL], [
    AC_PATH_PROG([$1], [$2])
    if test -z "[$$1]"; then
        AC_MSG_ERROR([m4_default([$3], [Cannot find [$2]])])
    fi
]) # _AC_OCAML_PATH_PROG_FATAL

# _AC_OCAML_PATH_PROG_NONFATAL(VARIABLE, PROGRAM, [MESSAGE])
# ----------------------------------------------------------
# wraps AC_PATH_PROG, issuing a warning if PROGRAM
# is not found, otherwise affects its path to VARIABLE
AC_DEFUN([_AC_OCAML_PATH_PROG_NONFATAL], [
    AC_PATH_PROG([$1], [$2])
    if test -z "[$$1]"; then
        AC_MSG_WARN([m4_default([$3], [Cannot find [$2]])])
    fi
]) # _AC_OCAML_PATH_PROG_NONFATAL

# _AC_OCAML_CHECK_VERSION(VARIABLE, PROGRAM)
# ------------------------------------------
# check than PROGRAM version is the same as the OCaml compiler,
# otherwise unset VARIABLE
AC_DEFUN([_AC_OCAML_CHECK_VERSION], [
    AC_MSG_CHECKING([for [$2] version])
    ac_ocaml_check_version=`$[$1] -version`
    AC_MSG_RESULT([$ac_ocaml_check_version])
    if test "$ac_ocaml_check_version" != "$OCAMLVERSION"; then
        unset [$1]
    fi
]) # _AC_OCAML_CHECK_VERSION

# _AC_OCAML_CHECK_VERSION_NONFATAL(VARIABLE, PROGRAM)
# ------------------------------------------
# wraps _AC_OCAML_CHECK_VERSION, issuing a warning if it fails
AC_DEFUN([_AC_OCAML_CHECK_VERSION_NONFATAL], [
    _AC_OCAML_CHECK_VERSION([$1], [$2])
    if test -z ["$$1"]; then
        AC_MSG_WARN([[$2] version differs from ocamlc, discarding])
    fi
]) # _AC_OCAML_CHECK_VERSION_NONFATAL

# _AC_OCAML_CHECK_VERSION_FATAL(VARIABLE, PROGRAM)
# ------------------------------------------
# wraps _AC_OCAML_CHECK_VERSION, issuing an error if it fails
AC_DEFUN([_AC_OCAML_CHECK_VERSION_FATAL], [
    _AC_OCAML_CHECK_VERSION([$1], [$2])
    if test -z ["$$1"]; then
        AC_MSG_ERROR([[$2] version differs from ocamlc, aborting])
    fi
]) # _AC_OCAML_CHECK_VERSION_FATAL 
