/* grep.h - interface to grep driver for searching subroutines.
   Copyright (C) 1992, 1998, 2001, 2007, 2009-2018 Free Software Foundation,
   Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#ifndef GREP_GREP_H
#define GREP_GREP_H 1

#include <stdbool.h>
#include <slbuf.h>
#include "../../match.h"

bool grepfile (struct grep_ctx *, int, char const *, bool, bool);
bool grepdesc (struct grep_ctx *, int, bool);

void parse_grep_colors (struct grep_ctx *, const char *);
void init_globals(struct grep_ctx *);
void clean_globals(struct grep_ctx *);

void color_cap_mt_fct (struct grep_ctx *ctx);
void color_cap_rv_fct (struct grep_ctx *ctx);
void color_cap_ne_fct (struct grep_ctx *ctx);

typedef struct
{
  char name[12];
  int syntax; /* used if compile == GEAcompile */
  compile_fp_t compile;
  execute_fp_t execute;
} matcher_t;

extern const matcher_t matchers[7];

#endif
