m4_dnl   p2.m4 - camlp2 bootstrap macros
m4_dnl   (C) 2015 M E Leypold
m4_dnl  
m4_dnl   This program is free software: you can redistribute it and/or modify
m4_dnl   it under the terms of the GNU General Public License as published by
m4_dnl   the Free Software Foundation, either version 3 of the License, or
m4_dnl   (at your option) any later version.
m4_dnl  
m4_dnl   This program is distributed in the hope that it will be useful,
m4_dnl   but WITHOUT ANY WARRANTY; without even the implied warranty of
m4_dnl   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
m4_dnl   GNU General Public License for more details.
m4_dnl  
m4_dnl   You should have received a copy of the GNU General Public License
m4_dnl   along with this program.  If not, see <http://www.gnu.org/licenses/>.
m4_dnl
m4_dnl
m4_divert(-1)m4_changecom(",")m4_changequote({{,}})
m4_define({{STRINGIFY}},{{{{"}}m4_changecom()m4_patsubst({{$1}},{{["]}},{{\\"}}){{"}}m4_changecom(",")}})
m4_define({{FEATURE}},{{m4_include($1.p2)
BEGIN_FEATURE}})
m4_divert(2)m4_dnl
