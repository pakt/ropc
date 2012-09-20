##### http://autoconf-archive.cryp.to/acltx_package.html
#
# SYNOPSIS
#
#   ACLTX_PACKAGE(PACKAGENAME,CLASSNAME,VARIABLETOSET[,ACTION-IF-FOUND[,ACTION-IF-NOT-FOUND]])
#
# DESCRIPTION
#
#   This macro try to compile a latex document using class CLASSNAME
#   and including package PACKAGENAME and set VARIABLETOSET to yes or
#   no If ACTION-IF-FOUND (and ACTION-IF-NOT-FOUND) are set to the
#   right action
#
# LAST MODIFICATION
#
#   2006-07-16
#
# COPYLEFT
#
#   Copyright (c) 2006 Boretti Mathieu <boretti@eig.unige.ch>
#
#   This library is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public License
#   as published by the Free Software Foundation; either version 2.1 of
#   the License, or (at your option) any later version.
#
#   This library is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
#   Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this library; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
#   02110-1301 USA

AC_DEFUN([ACLTX_PACKAGE],[
ACLTX_PACKAGE_LOCATION($1.sty,$3_location)
if test "[$]$3_location" = "no" ; then
    AC_MSG_WARN([Unable to locate the $1.sty file])
    [ac_cv_latex_]translit($1,[-],[_])[_]translit($2,[-],[_])="no";
else
if test "$[ac_cv_latex_class_]translit($2,[-],[_])" = "" ;
then
	ACLTX_CLASS($2,boretti_classesansparametre)
	export boretti_classesansparametre;
else
	boretti_classesansparametre=$[ac_cv_latex_class_]translit($2,[-],[_]) ;
	export boretti_classesansparemetre;
fi;
if test $boretti_classesansparametre = "no" ;
then
    AC_MSG_ERROR([Unable to find $2 class])
fi
AC_CACHE_CHECK([for usability of package $1 in class $2],[ac_cv_latex_]translit($1,[-],[_])[_]translit($2,[-],[_]),[
_ACLTX_TEST([\documentclass{$2}
\usepackage{$1}
\begin{document}
\end{document}],[ac_cv_latex_]translit($1,[-],[_])[_]translit($2,[-],[_]))
])
fi
$3=$[ac_cv_latex_]translit($1,[-],[_])[_]translit($2,[-],[_]); export $3;
AC_SUBST($3)
ifelse($#,3,[],$#,4,[
    if test "[$]$3" = "yes" ;
    then
        $4
    fi
],$#,5,[
    ifelse($4,[],[
        if test "[$]$3" = "no" ;
        then
            $5
        fi
    ],[
        if test "[$]$3" = "yes" ;
        then
            $4
        else
            $5
        fi
    ])
])
])
