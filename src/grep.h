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

/* The following flags are exported from grep for the matchers
   to look at. */
extern bool match_icase;	/* -i */
extern bool match_words;	/* -w */
extern bool match_lines;	/* -x */
extern char eolbyte;		/* -z */

extern char const *pattern_file_name (size_t, size_t *);

typedef void *(*compile_fp_t) (char *, size_t, unsigned long int);
typedef size_t (*execute_fp_t) (void *, char const *, size_t, size_t *,
        char const *);

bool grepfile (struct thread *td, struct slbuf *slbuf, int dirdesc, char const *name, bool follow, bool command_line);
bool grepdesc (struct thread *, struct slbuf *slbuf, int, bool);

void parse_grep_colors (void);
void init_globals(void);
void clean_globals(void);

typedef struct
{
  char name[12];
  int syntax; /* used if compile == GEAcompile */
  compile_fp_t compile;
  execute_fp_t execute;
} matcher_t;

extern const matcher_t matchers[7];

#endif
