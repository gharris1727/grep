/* grep.c - main driver file for grep.
   Copyright (C) 1992, 1997-2002, 2004-2018 Free Software Foundation, Inc.

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

/* Written July 1992 by Mike Haertel.  */

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <wchar.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include "system.h"

#include "inttypes.h"
#include "argmatch.h"
#include "c-ctype.h"
#include "c-stack.h"
#include "closeout.h"
#include "colorize.h"
#include "die.h"
#include "error.h"
#include "exclude.h"
#include "exitfail.h"
#include "fcntl-safer.h"
//#include "fts_.h"
//#include "getopt.h"
#include "getprogname.h"
#include "grep.h"
#include "intprops.h"
#include "propername.h"
#include "quote.h"
#include "safe-read.h"
#include "search.h"
#include "version-etc.h"
#include "xalloc.h"
#include "xbinary-io.h"
//#include "xstrtol.h"

#include <slbuf.h>
#include <sys/proc.h>
#include <sys/syscallsubr.h>


enum { SEP_CHAR_SELECTED = ':' };
enum { SEP_CHAR_REJECTED = '-' };

#define AUTHORS \
  proper_name ("Mike Haertel"), \
  _("others, see <http://git.sv.gnu.org/cgit/grep.git/tree/AUTHORS>")

/* When stdout is connected to a regular file, save its stat
   information here, so that we can automatically skip it, thus
   avoiding a potential (racy) infinite loop.  */
static struct stat out_stat;


#if HAVE_ASAN
/* Record the starting address and length of the sole poisoned region,
   so that we can unpoison it later, just before each following read.  */
static void const *poison_buf;
static size_t poison_len;

static void
clear_asan_poison (void)
{
  if (poison_buf)
    __asan_unpoison_memory_region (poison_buf, poison_len);
}

static void
asan_poison (void const *addr, size_t size)
{
  poison_buf = addr;
  poison_len = size;

  __asan_poison_memory_region (poison_buf, poison_len);
}
#else
static void clear_asan_poison (void) { }
static void asan_poison (void const volatile *addr, size_t size) { }
#endif

/* The context and logic for choosing default --color screen attributes
   (foreground and background colors, etc.) are the following.
      -- There are eight basic colors available, each with its own
         nominal luminosity to the human eye and foreground/background
         codes (black [0 %, 30/40], blue [11 %, 34/44], red [30 %, 31/41],
         magenta [41 %, 35/45], green [59 %, 32/42], cyan [70 %, 36/46],
         yellow [89 %, 33/43], and white [100 %, 37/47]).
      -- Sometimes, white as a background is actually implemented using
         a shade of light gray, so that a foreground white can be visible
         on top of it (but most often not).
      -- Sometimes, black as a foreground is actually implemented using
         a shade of dark gray, so that it can be visible on top of a
         background black (but most often not).
      -- Sometimes, more colors are available, as extensions.
      -- Other attributes can be selected/deselected (bold [1/22],
         underline [4/24], standout/inverse [7/27], blink [5/25], and
         invisible/hidden [8/28]).  They are sometimes implemented by
         using colors instead of what their names imply; e.g., bold is
         often achieved by using brighter colors.  In practice, only bold
         is really available to us, underline sometimes being mapped by
         the terminal to some strange color choice, and standout best
         being left for use by downstream programs such as less(1).
      -- We cannot assume that any of the extensions or special features
         are available for the purpose of choosing defaults for everyone.
      -- The most prevalent default terminal backgrounds are pure black
         and pure white, and are not necessarily the same shades of
         those as if they were selected explicitly with SGR sequences.
         Some terminals use dark or light pictures as default background,
         but those are covered over by an explicit selection of background
         color with an SGR sequence; their users will appreciate their
         background pictures not be covered like this, if possible.
      -- Some uses of colors attributes is to make some output items
         more understated (e.g., context lines); this cannot be achieved
         by changing the background color.
      -- For these reasons, the grep color defaults should strive not
         to change the background color from its default, unless it's
         for a short item that should be highlighted, not understated.
      -- The grep foreground color defaults (without an explicitly set
         background) should provide enough contrast to be readable on any
         terminal with either a black (dark) or white (light) background.
         This only leaves red, magenta, green, and cyan (and their bold
         counterparts) and possibly bold blue.  */
/* The color strings used for matched text.
   The user can overwrite them using the deprecated
   environment variable GREP_COLOR or the new GREP_COLORS.  */

/* Select Graphic Rendition (SGR, "\33[...m") strings.  */
/* Also Erase in Line (EL) to Right ("\33[K") by default.  */
/*    Why have EL to Right after SGR?
         -- The behavior of line-wrapping when at the bottom of the
            terminal screen and at the end of the current line is often
            such that a new line is introduced, entirely cleared with
            the current background color which may be different from the
            default one (see the boolean back_color_erase terminfo(5)
            capability), thus scrolling the display by one line.
            The end of this new line will stay in this background color
            even after reverting to the default background color with
            "\33[m', unless it is explicitly cleared again with "\33[K"
            (which is the behavior the user would instinctively expect
            from the whole thing).  There may be some unavoidable
            background-color flicker at the end of this new line because
            of this (when timing with the monitor's redraw is just right).
         -- The behavior of HT (tab, "\t") is usually the same as that of
            Cursor Forward Tabulation (CHT) with a default parameter
            of 1 ("\33[I"), i.e., it performs pure movement to the next
            tab stop, without any clearing of either content or screen
            attributes (including background color); try
               printf 'asdfqwerzxcv\rASDF\tZXCV\n'
            in a bash(1) shell to demonstrate this.  This is not what the
            user would instinctively expect of HT (but is ok for CHT).
            The instinctive behavior would include clearing the terminal
            cells that are skipped over by HT with blank cells in the
            current screen attributes, including background color;
            the boolean dest_tabs_magic_smso terminfo(5) capability
            indicates this saner behavior for HT, but only some rare
            terminals have it (although it also indicates a special
            glitch with standout mode in the Teleray terminal for which
            it was initially introduced).  The remedy is to add "\33K"
            after each SGR sequence, be it START (to fix the behavior
            of any HT after that before another SGR) or END (to fix the
            behavior of an HT in default background color that would
            follow a line-wrapping at the bottom of the screen in another
            background color, and to complement doing it after START).
            Piping grep's output through a pager such as less(1) avoids
            any HT problems since the pager performs tab expansion.

      Generic disadvantages of this remedy are:
         -- Some very rare terminals might support SGR but not EL (nobody
            will use "grep --color" on a terminal that does not support
            SGR in the first place).
         -- Having these extra control sequences might somewhat complicate
            the task of any program trying to parse "grep --color"
            output in order to extract structuring information from it.
      A specific disadvantage to doing it after SGR START is:
         -- Even more possible background color flicker (when timing
            with the monitor's redraw is just right), even when not at the
            bottom of the screen.
      There are no additional disadvantages specific to doing it after
      SGR END.

      It would be impractical for GNU grep to become a full-fledged
      terminal program linked against ncurses or the like, so it will
      not detect terminfo(5) capabilities.  */

#define PRINTF_BUF_SZ 1024
static char printf_buf[PRINTF_BUF_SZ];

static void __attribute__((format (printf, 2, 3)))
printf_errno (struct grep_ctx *ctx, char const *format, ...)
{
    va_list ap;
    va_start (ap, format);

    ssize_t size = 0;
    if ((size = vsnprintf(printf_buf, PRINTF_BUF_SZ, format, ap)) >= 0) {
        slbuf_write(ctx->out, printf_buf, MIN(size, PRINTF_BUF_SZ));
    }
    va_end (ap);
}

/* SGR utility functions.  */
static void
pr_sgr_start (struct grep_ctx *ctx, char const *s)
{
  if (*s) {

      if (ctx->options[SGR_ALT]) {
          printf_errno (ctx, "\33[%sm", s);
      } else {
          printf_errno (ctx, "\33[%sm\33[K", s);
      }
  }
}
static void
pr_sgr_end (struct grep_ctx *ctx, char const *s)
{
  if (*s) {
      if (ctx->options[SGR_ALT]) {
          printf_errno (ctx, "\33[m");
      } else {
          printf_errno (ctx, "\33[m\33[K");
      }
  }
}
static void
pr_sgr_start_if (struct grep_ctx *ctx, char const *s)
{
  if (ctx->options[COLOR_ENABLE])
    pr_sgr_start (ctx, s);
}
static void
pr_sgr_end_if (struct grep_ctx *ctx, char const *s)
{
  if (ctx->options[COLOR_ENABLE])
    pr_sgr_end (ctx, s);
}

void
color_cap_mt_fct (struct grep_ctx *ctx)
{
  /* Our caller just set selected_match_color.  */
  ctx->context_match_color = ctx->selected_match_color;
}

void
color_cap_rv_fct (struct grep_ctx *ctx)
{
  /* By this point, it was 1 (or already -1).  */
  ctx->options[COLOR_ENABLE] = -1;  /* That's still != 0.  */
}

void
color_cap_ne_fct (struct grep_ctx *ctx)
{
    ctx->options[SGR_ALT] = true;
}

static void
putchar_errno (struct grep_ctx *ctx, int c)
{
    char cc = (char) c;
    slbuf_write(ctx->out, &cc, 1);
}

static void
fputs_errno (struct grep_ctx *ctx, char const *s)
{
    slbuf_write(ctx->out, s, strlen(s));
}

static void
fwrite_errno (struct grep_ctx *ctx, void const *ptr, size_t size, size_t nmemb)
{
    slbuf_write(ctx->out, ptr, size*nmemb);
}

static void
fflush_errno (void)
{
}

/* Non-boolean long options that have no corresponding short equivalents.  */
enum
{
  BINARY_FILES_OPTION = CHAR_MAX + 1,
  COLOR_OPTION,
  EXCLUDE_DIRECTORY_OPTION,
  EXCLUDE_OPTION,
  EXCLUDE_FROM_OPTION,
  GROUP_SEPARATOR_OPTION,
  INCLUDE_OPTION,
  LINE_BUFFERED_OPTION,
  LABEL_OPTION
};

enum directories_type
  {
    READ_DIRECTORIES = 2,
    RECURSE_DIRECTORIES,
    SKIP_DIRECTORIES
  };

static enum directories_type directories = READ_DIRECTORIES;

/* How to handle devices. */
static enum
  {
    READ_COMMAND_LINE_DEVICES,
    READ_DEVICES,
    SKIP_DEVICES
  } devices = READ_COMMAND_LINE_DEVICES;

static bool
is_device_mode (mode_t m)
{
  return S_ISCHR (m) || S_ISBLK (m) || S_ISSOCK (m) || S_ISFIFO (m);
}

static bool
skip_devices (bool command_line)
{
  return (devices == SKIP_DEVICES
          || ((devices == READ_COMMAND_LINE_DEVICES) & !command_line));
}

/* Return if ST->st_size is defined.  Assume the file is not a
   symbolic link.  */
static bool
usable_st_size (struct stat const *st)
{
  return S_ISREG (st->st_mode) || S_TYPEISSHM (st) || S_TYPEISTMO (st);
}

/* Lame substitutes for SEEK_DATA and SEEK_HOLE on platforms lacking them.
   Do not rely on these finding data or holes if they equal SEEK_SET.  */
#ifndef SEEK_DATA
enum { SEEK_DATA = SEEK_SET };
#endif
#ifndef SEEK_HOLE
enum { SEEK_HOLE = SEEK_SET };
#endif

static char const *
input_filename (struct grep_ctx *ctx)
{
  if (!ctx->filename)
    ctx->filename = _("(standard input)");
  return ctx->filename;
}

/* Unless requested, diagnose an error about the input file.  */
static void
suppressible_error (struct grep_ctx *ctx, int errnum)
{
  if (! ctx->options[SUPPRESS_ERRORS])
    error (0, errnum, "%s", input_filename (ctx));
  ctx->errseen = true;
}

/* A cast to TYPE of VAL.  Use this when TYPE is a pointer type, VAL
   is properly aligned for TYPE, and 'gcc -Wcast-align' cannot infer
   the alignment and would otherwise complain about the cast.  */
#if 4 < __GNUC__ + (6 <= __GNUC_MINOR__)
# define CAST_ALIGNED(type, val)                           \
    ({ __typeof__ (val) val_ = val;                        \
       _Pragma ("GCC diagnostic push")                     \
       _Pragma ("GCC diagnostic ignored \"-Wcast-align\"") \
       (type) val_;                                        \
       _Pragma ("GCC diagnostic pop")                      \
    })
#else
# define CAST_ALIGNED(type, val) ((type) (val))
#endif

/* An unsigned type suitable for fast matching.  */
typedef uintmax_t uword;

/* A mask to test for unibyte characters, with the pattern repeated to
   fill a uword.  For a multibyte character encoding where
   all bytes are unibyte characters, this is 0.  For UTF-8, this is
   0x808080....  For encodings where unibyte characters have no discerned
   pattern, this is all 1s.  The unsigned char C is a unibyte
   character if C & UNIBYTE_MASK is zero.  If the uword W is the
   concatenation of bytes, the bytes are all unibyte characters
   if W & UNIBYTE_MASK is zero.  */

static void
initialize_unibyte_mask (struct grep_ctx *ctx)
{
  /* For each encoding error I that MASK does not already match,
     accumulate I's most significant 1 bit by ORing it into MASK.
     Although any 1 bit of I could be used, in practice high-order
     bits work better.  */
  unsigned char mask = 0;
  int ms1b = 1;
  for (int i = 1; i <= UCHAR_MAX; i++)
    if ((ctx->localeinfo.sbclen[i] != 1) & ! (mask & i))
      {
        while (ms1b * 2 <= i)
          ms1b *= 2;
        mask |= ms1b;
      }

  /* Now MASK will detect any encoding-error byte, although it may
     cry wolf and it may not be optimal.  Build a uword-length mask by
     repeating MASK.  */
  uword uword_max = -1;
  ctx->unibyte_mask = uword_max / UCHAR_MAX * mask;
}

/* Skip the easy bytes in a buffer that is guaranteed to have a sentinel
   that is not easy, and return a pointer to the first non-easy byte.
   The easy bytes all have UNIBYTE_MASK off.  */
static char const * _GL_ATTRIBUTE_PURE
skip_easy_bytes (struct grep_ctx *ctx, char const *buf)
{
  /* Search a byte at a time until the pointer is aligned, then a
     uword at a time until a match is found, then a byte at a time to
     identify the exact byte.  The uword search may go slightly past
     the buffer end, but that's benign.  */
  char const *p;
  uword const *s;
  for (p = buf; (uintptr_t) p % sizeof (uword) != 0; p++)
    if (to_uchar (*p) & ctx->unibyte_mask)
      return p;
  for (s = CAST_ALIGNED (uword const *, p); ! (*s & ctx->unibyte_mask); s++)
    continue;
  for (p = (char const *) s; ! (to_uchar (*p) & ctx->unibyte_mask); p++)
    continue;
  return p;
}

/* Return true if BUF, of size SIZE, has an encoding error.
   BUF must be followed by at least sizeof (uword) bytes,
   the first of which may be modified.  */
static bool
buf_has_encoding_errors (struct grep_ctx *ctx, char *buf, size_t size)
{
  if (! ctx->unibyte_mask)
    return false;

  mbstate_t mbs = { {0} };
  size_t clen;

  buf[size] = -1;
  for (char const *p = buf; (p = skip_easy_bytes (ctx, p)) < buf + size; p += clen)
    {
      clen = mbrlen (p, buf + size - p, &mbs);
      if ((size_t) -2 <= clen)
        return true;
    }

  return false;
}


/* Return true if BUF, of size SIZE, has a null byte.
   BUF must be followed by at least one byte,
   which may be arbitrarily written to or read from.  */
static bool
buf_has_nulls (char *buf, size_t size)
{
  buf[size] = 0;
  return strlen (buf) != size;
}

/* Return true if a file is known to contain null bytes.
   SIZE bytes have already been read from the file
   with descriptor FD and status ST.  */
static bool
file_must_have_nulls (struct grep_ctx *ctx, size_t size, int fd, struct stat const *st)
{
  /* If the file has holes, it must contain a null byte somewhere.  */
  if (SEEK_HOLE != SEEK_SET && !ctx->seek_failed
      && usable_st_size (st) && size < st->st_size)
    {
      off_t cur = size;
      if (O_BINARY || fd == STDIN_FILENO)
        {
          cur = kern_lseek (ctx->td, fd, 0, SEEK_CUR);
          if (cur < 0)
            return false;
        }

      /* Look for a hole after the current location.  */
      off_t hole_start = kern_lseek (ctx->td, fd, cur, SEEK_HOLE);
      if (0 <= hole_start)
        {
          if (kern_lseek (ctx->td, fd, cur, SEEK_SET) < 0)
            suppressible_error (ctx, EFAULT);
          if (hole_start < st->st_size)
            return true;
        }
    }

  return false;
}

/* Hairy buffering mechanism for grep.  The intent is to keep
   all reads aligned on a page boundary and multiples of the
   page size, unless a read yields a partial page.  */

static char *buffer;		/* Base of buffer. */
static size_t bufalloc;		/* Allocated buffer size, counting slop. */
enum { INITIAL_BUFSIZE = 32768 }; /* Initial buffer size, not counting slop. */
static int bufdesc;		/* File descriptor. */
static char *bufbeg;		/* Beginning of user-visible stuff. */
static char *buflim;		/* Limit of user-visible stuff. */
static size_t pagesize;		/* alignment of memory pages */
static off_t bufoffset;		/* Read offset.  */
static off_t after_last_match;	/* Pointer after last matching line that
                                   would have been output if we were
                                   outputting characters. */
static bool skip_nuls;		/* Skip '\0' in data.  */
static bool skip_empty_lines;	/* Skip empty lines in data.  */
static uintmax_t totalnl;	/* Total newline count before lastnl. */

/* Return VAL aligned to the next multiple of ALIGNMENT.  VAL can be
   an integer or a pointer.  Both args must be free of side effects.  */
#define ALIGN_TO(val, alignment) \
  ((size_t) (val) % (alignment) == 0 \
   ? (val) \
   : (val) + ((alignment) - (size_t) (val) % (alignment)))

/* Add two numbers that count input bytes or lines, and report an
   error if the addition overflows.  */
static uintmax_t
add_count (uintmax_t a, uintmax_t b)
{
  uintmax_t sum = a + b;
  if (sum < a)
    die (EXIT_TROUBLE, 0, _("input is too large to count"));
  return sum;
}

/* Return true if BUF (of size SIZE) is all zeros.  */
static bool
all_zeros (char const *buf, size_t size)
{
  for (char const *p = buf; p < buf + size; p++)
    if (*p)
      return false;
  return true;
}

/* Reset the buffer for a new file, returning false if we should skip it.
   Initialize on the first time through. */
static bool
reset (struct grep_ctx *ctx, int fd, struct stat const *st)
{
  bufbeg = buflim = ALIGN_TO (buffer + 1, pagesize);
  bufbeg[-1] = ctx->options[NULL_BOUND] ? '\0' : '\n';
  bufdesc = fd;
  bufoffset = fd == STDIN_FILENO ? kern_lseek (ctx->td, fd, 0, SEEK_CUR) : 0;
  ctx->seek_failed = bufoffset < 0;

  /* Assume SEEK_DATA fails if SEEK_CUR does.  */
  ctx->seek_data_failed = ctx->seek_failed;

  if (ctx->seek_failed)
    {
          suppressible_error (ctx, EFAULT);
          return false;
    }
  return true;
}

/* Read new stuff into the buffer, saving the specified
   amount of old stuff.  When we're done, 'bufbeg' points
   to the beginning of the buffer contents, and 'buflim'
   points just after the end.  Return false if there's an error.  */
static bool
fillbuf (struct grep_ctx *ctx, size_t save, struct stat const *st)
{
  size_t fillsize;
  bool cc = true;
  char *readbuf;
  size_t readsize;

  /* Offset from start of buffer to start of old stuff
     that we want to save.  */
  size_t saved_offset = buflim - save - buffer;

  if (pagesize <= buffer + bufalloc - sizeof (uword) - buflim)
    {
      readbuf = buflim;
      bufbeg = buflim - save;
    }
  else
    {
      size_t minsize = save + pagesize;
      size_t newsize;
      size_t newalloc;
      char *newbuf;

      /* Grow newsize until it is at least as great as minsize.  */
      for (newsize = bufalloc - pagesize - sizeof (uword);
           newsize < minsize;
           newsize *= 2)
        if ((SIZE_MAX - pagesize - sizeof (uword)) / 2 < newsize)
          xalloc_die ();

      /* Try not to allocate more memory than the file size indicates,
         as that might cause unnecessary memory exhaustion if the file
         is large.  However, do not use the original file size as a
         heuristic if we've already read past the file end, as most
         likely the file is growing.  */
      if (usable_st_size (st))
        {
          off_t to_be_read = st->st_size - bufoffset;
          off_t maxsize_off = save + to_be_read;
          if (0 <= to_be_read && to_be_read <= maxsize_off
              && maxsize_off == (size_t) maxsize_off
              && minsize <= (size_t) maxsize_off
              && (size_t) maxsize_off < newsize)
            newsize = maxsize_off;
        }

      /* Add enough room so that the buffer is aligned and has room
         for byte sentinels fore and aft, and so that a uword can
         be read aft.  */
      newalloc = newsize + pagesize + sizeof (uword);

      newbuf = bufalloc < newalloc ? xmalloc (bufalloc = newalloc) : buffer;
      readbuf = ALIGN_TO (newbuf + 1 + save, pagesize);
      bufbeg = readbuf - save;
      memmove (bufbeg, buffer + saved_offset, save);
      bufbeg[-1] = ctx->options[NULL_BOUND] ? '\0' : '\n';
      if (newbuf != buffer)
        {
          free (buffer);
          buffer = newbuf;
        }
    }

  clear_asan_poison ();

  readsize = buffer + bufalloc - sizeof (uword) - readbuf;
  readsize -= readsize % pagesize;

  while (true)
    {

        struct uio auio;
        struct iovec aiov;

        aiov.iov_base = readbuf;
        aiov.iov_len = readsize;
        auio.uio_iov = &aiov;
        auio.uio_iovcnt = 1;
        auio.uio_resid = readsize;
        auio.uio_segflg = UIO_SYSSPACE;

        if (kern_readv(ctx->td, bufdesc, &auio)) {
            break;
        }
        fillsize = readsize - auio.uio_resid;
      bufoffset += fillsize;

      if (((fillsize == 0) | !skip_nuls) || !all_zeros (readbuf, fillsize))
        break;
      totalnl = add_count (totalnl, fillsize);

      if (SEEK_DATA != SEEK_SET && !ctx->seek_data_failed)
        {
          /* Solaris SEEK_DATA fails with errno == ENXIO in a hole at EOF.  */
          off_t data_start = kern_lseek (ctx->td, bufdesc, bufoffset, SEEK_DATA);
          if (data_start < 0 
              && usable_st_size (st) && bufoffset < st->st_size)
            data_start = kern_lseek (ctx->td, bufdesc, 0, SEEK_END);

          if (data_start < 0)
            ctx->seek_data_failed = true;
          else
            {
              totalnl = add_count (totalnl, data_start - bufoffset);
              bufoffset = data_start;
            }
        }
    }

  buflim = readbuf + fillsize;

  /* Initialize the following word, because skip_easy_bytes and some
     matchers read (but do not use) those bytes.  This avoids false
     positive reports of these bytes being used uninitialized.  */
  memset (buflim, 0, sizeof (uword));

  /* Mark the part of the buffer not filled by the read or set by
     the above memset call as ASAN-poisoned.  */
  asan_poison (buflim + sizeof (uword),
               bufalloc - (buflim - buffer) - sizeof (uword));

  return cc;
}

/* Flags controlling the style of output. */
static enum
{
  BINARY_BINARY_FILES,
  TEXT_BINARY_FILES,
  WITHOUT_MATCH_BINARY_FILES
} binary_files;		/* How to handle binary files.  */

/* Options for output as a list of matching/non-matching files */
static enum
{
  LISTFILES_NONE,
  LISTFILES_MATCHING,
  LISTFILES_NONMATCHING,
} list_files;

static int filename_mask;	/* If zero, output nulls after filenames.  */
static bool out_quiet;		/* Suppress all normal output. */
static bool out_invert;		/* Print nonmatching stuff. */
static int out_file;		/* Print filenames. */
static bool out_line;		/* Print line numbers. */
static bool out_byte;		/* Print byte offsets. */
static bool line_buffered;	/* Use line buffering.  */

/* Internal variables to keep track of byte count, context, etc. */
static uintmax_t totalcc;	/* Total character count before bufbeg. */
static char const *lastnl;	/* Pointer after last newline counted. */
static char *lastout;		/* Pointer after last character output;
                                   NULL if no character has been output
                                   or if it's conceptually before bufbeg. */
static intmax_t outleft;	/* Maximum number of selected lines.  */
static intmax_t pending;	/* Pending lines of output.
                                   Always kept 0 if out_quiet is true.  */
static bool done_on_match;	/* Stop scanning file on first match.  */
static bool dev_null_output;	/* Stdout is known to be /dev/null.  */
static bool binary;		/* Use binary rather than text I/O.  */

static void
nlscan (struct grep_ctx *ctx, char const *lim)
{
  size_t newlines = 0;
  char const *beg;
  char eolbyte = ctx->options[NULL_BOUND] ? '\0' : '\n';
  for (beg = lastnl; beg < lim; beg++)
    {
      beg = memchr (beg, eolbyte, lim - beg);
      if (!beg)
        break;
      newlines++;
    }
  totalnl = add_count (totalnl, newlines);
  lastnl = lim;
}

/* Print the current filename.  */
static void
print_filename (struct grep_ctx *ctx)
{
  pr_sgr_start_if (ctx, ctx->filename_color);
  fputs_errno (ctx, input_filename (ctx));
  pr_sgr_end_if (ctx, ctx->filename_color);
}

/* Print a character separator.  */
static void
print_sep (struct grep_ctx *ctx, char sep)
{
  pr_sgr_start_if (ctx, ctx->sep_color);
  putchar_errno (ctx, sep);
  pr_sgr_end_if (ctx, ctx->sep_color);
}

/* Print a line number or a byte offset.  */
static void
print_offset (struct grep_ctx *ctx, uintmax_t pos, const char *color)
{
  pr_sgr_start_if (ctx, color);
  printf_errno (ctx, ("%*"PRIuMAX), ctx->offset_width, pos);
  pr_sgr_end_if (ctx, color);
}

/* Print a whole line head (filename, line, byte).  The output data
   starts at BEG and contains LEN bytes; it is followed by at least
   sizeof (uword) bytes, the first of which may be temporarily modified.
   The output data comes from what is perhaps a larger input line that
   goes until LIM, where LIM[-1] is an end-of-line byte.  Use SEP as
   the separator on output.

   Return true unless the line was suppressed due to an encoding error.  */

static bool
print_line_head (struct grep_ctx *ctx, char *beg, size_t len, char const *lim, char sep)
{
  if (binary_files != TEXT_BINARY_FILES)
    {
      char ch = beg[len];
      bool encoding_errors = buf_has_encoding_errors (ctx, beg, len);
      beg[len] = ch;
      if (encoding_errors)
        {
          ctx->encoding_error_output = true;
          return false;
        }
    }

  if (out_file)
    {
      print_filename (ctx);
      if (filename_mask)
        print_sep (ctx, sep);
      else
        putchar_errno (ctx, 0);
    }

  if (out_line)
    {
      if (lastnl < lim)
        {
          nlscan (ctx, beg);
          totalnl = add_count (totalnl, 1);
          lastnl = lim;
        }
      print_offset (ctx, totalnl, ctx->line_num_color);
      print_sep (ctx, sep);
    }

  if (out_byte)
    {
      uintmax_t pos = add_count (totalcc, beg - bufbeg);
      print_offset (ctx, pos, ctx->byte_num_color);
      print_sep (ctx, sep);
    }

  if (ctx->options[TAB_ALIGN] && (out_file | out_line | out_byte) && len != 0)
    putchar_errno (ctx, '\t');

  return true;
}

static char *
print_line_middle (struct grep_ctx *ctx, char *beg, char *lim,
                   const char *line_color, const char *match_color)
{
  size_t match_size;
  size_t match_offset;
  char *cur;
  char *mid = NULL;
  char *b;

  for (cur = beg;
       (cur < lim
        && ((match_offset = ctx->execute (ctx, ctx->compiled_pattern, beg, lim - beg,
                                     &match_size, cur)) != (size_t) -1));
       cur = b + match_size)
    {
      b = beg + match_offset;


      /* Avoid matching the empty line at the end of the buffer. */
      if (b == lim)
        break;

      /* Avoid hanging on grep --color "" foo */
      if (match_size == 0)
        {
          /* Make minimal progress; there may be further non-empty matches.  */
          /* XXX - Could really advance by one whole multi-octet character.  */
          match_size = 1;
          if (!mid)
            mid = cur;
        }
      else
        {
          /* This function is called on a matching line only,
             but is it selected or rejected/context?  */
          if (ctx->options[ONLY_MATCH])
            {
              char sep = out_invert ? SEP_CHAR_REJECTED : SEP_CHAR_SELECTED;
              if (! print_line_head (ctx, b, match_size, lim, sep))
                return NULL;
            }
          else
            {
              pr_sgr_start (ctx, line_color);
              if (mid)
                {
                  cur = mid;
                  mid = NULL;
                }
              fwrite_errno (ctx, cur, 1, b - cur);
            }

          pr_sgr_start_if (ctx, match_color);
          fwrite_errno (ctx, b, 1, match_size);
          pr_sgr_end_if (ctx, match_color);
          if (ctx->options[ONLY_MATCH])
            putchar_errno (ctx, ctx->options[NULL_BOUND] ? '\0' : '\n');
        }
    }

  if (ctx->options[ONLY_MATCH])
    cur = lim;
  else if (mid)
    cur = mid;

  return cur;
}

static char *
print_line_tail (struct grep_ctx *ctx, char *beg, const char *lim, const char *line_color)
{
  size_t eol_size;
  size_t tail_size;

  eol_size   = (lim > beg && lim[-1] == (ctx->options[NULL_BOUND] ? '\0' : '\n'));
  eol_size  += (lim - eol_size > beg && lim[-(1 + eol_size)] == '\r');
  tail_size  =  lim - eol_size - beg;

  if (tail_size > 0)
    {
      pr_sgr_start (ctx, line_color);
      fwrite_errno (ctx, beg, 1, tail_size);
      beg += tail_size;
      pr_sgr_end (ctx, line_color);
    }

  return beg;
}

static void
prline (struct grep_ctx *ctx, char *beg, char *lim, char sep)
{
  bool matching;
  const char *line_color;
  const char *match_color;

  if (!ctx->options[ONLY_MATCH])
    if (! print_line_head (ctx, beg, lim - beg - 1, lim, sep))
      return;

  matching = (sep == SEP_CHAR_SELECTED) ^ out_invert;

  if (ctx->options[COLOR_ENABLE])
    {
      line_color = (((sep == SEP_CHAR_SELECTED)
                     ^ (out_invert && (ctx->options[COLOR_ENABLE] < 0)))
                    ? ctx->selected_line_color  : ctx->context_line_color);
      match_color = (sep == SEP_CHAR_SELECTED
                     ? ctx->selected_match_color : ctx->context_match_color);
    }
  else
    line_color = match_color = NULL; /* Shouldn't be used.  */

  if ((ctx->options[ONLY_MATCH] && matching)
      || (ctx->options[COLOR_ENABLE] && (*line_color || *match_color)))
    {
      /* We already know that non-matching lines have no match (to colorize). */
      if (matching && (ctx->options[ONLY_MATCH] || *match_color))
        {
          beg = print_line_middle (ctx, beg, lim, line_color, match_color);
          if (! beg)
            return;
        }

      if (!ctx->options[ONLY_MATCH] && *line_color)
        {
          /* This code is exercised at least when grep is invoked like this:
             echo k| GREP_COLORS='sl=01;32' src/grep k --color=always  */
          beg = print_line_tail (ctx, beg, lim, line_color);
        }
    }

  if (!ctx->options[ONLY_MATCH] && lim > beg)
    fwrite_errno (ctx, beg, 1, lim - beg);

  if (line_buffered)
    fflush_errno ();

  lastout = lim;
}

/* Print pending lines of trailing context prior to LIM.  */
static void
prpending (struct grep_ctx *ctx, char const *lim)
{
  if (!lastout)
    lastout = bufbeg;
  char eolbyte = ctx->options[NULL_BOUND] ? '\0' : '\n';
  for (; 0 < pending && lastout < lim; pending--)
    {
      char *nl = memchr (lastout, eolbyte, lim - lastout);
      prline (ctx, lastout, nl + 1, SEP_CHAR_REJECTED);
    }
}

/* Output the lines between BEG and LIM.  Deal with context.  */
static void
prtext (struct grep_ctx *ctx, char *beg, char *lim)
{
  static bool used;	/* Avoid printing SEP_STR_GROUP before any output.  */
  char eol = ctx->options[NULL_BOUND] ? '\0' : '\n';

  if (!out_quiet && pending > 0)
    prpending (ctx, beg);

  char *p = beg;

  if (!out_quiet)
    {
      /* Deal with leading context.  */
      char const *bp = lastout ? lastout : bufbeg;
      intmax_t i;
      for (i = 0; i < ctx->options[CONTEXT_BEFORE]; ++i)
        if (p > bp)
          do
            --p;
          while (p[-1] != eol);

      /* Print the group separator unless the output is adjacent to
         the previous output in the file.  */
      if ((0 <= ctx->options[CONTEXT_BEFORE] || 0 <= ctx->options[CONTEXT_AFTER]) && used
          && p != lastout && ctx->group_separator)
        {
          pr_sgr_start_if (ctx, ctx->sep_color);
          fputs_errno (ctx, ctx->group_separator);
          pr_sgr_end_if (ctx, ctx->sep_color);
          putchar_errno (ctx, '\n');
        }

      while (p < beg)
        {
          char *nl = memchr (p, eol, beg - p);
          nl++;
          prline (ctx, p, nl, SEP_CHAR_REJECTED);
          p = nl;
        }
    }

  intmax_t n;
  if (out_invert)
    {
      /* One or more lines are output.  */
      for (n = 0; p < lim && n < outleft; n++)
        {
          char *nl = memchr (p, eol, lim - p);
          nl++;
          if (!out_quiet)
            prline (ctx, p, nl, SEP_CHAR_SELECTED);
          p = nl;
        }
    }
  else
    {
      /* Just one line is output.  */
      if (!out_quiet)
        prline (ctx, beg, lim, SEP_CHAR_SELECTED);
      n = 1;
      p = lim;
    }

  after_last_match = bufoffset - (buflim - p);
  pending = out_quiet ? 0 : MAX (0, ctx->options[CONTEXT_AFTER]);
  used = true;
  outleft -= n;
}

/* Replace all NUL bytes in buffer P (which ends at LIM) with EOL.
   This avoids running out of memory when binary input contains a long
   sequence of zeros, which would otherwise be considered to be part
   of a long line.  P[LIM] should be EOL.  */
static void
zap_nuls (char *p, char *lim, char eol)
{
  if (eol)
    while (true)
      {
        *lim = '\0';
        p += strlen (p);
        *lim = eol;
        if (p == lim)
          break;
        do
          *p++ = eol;
        while (!*p);
      }
}

/* Scan the specified portion of the buffer, matching lines (or
   between matching lines if OUT_INVERT is true).  Return a count of
   lines printed.  Replace all NUL bytes with NUL_ZAPPER as we go.  */
static intmax_t
grepbuf (struct grep_ctx *ctx, char *beg, char const *lim)
{
  intmax_t outleft0 = outleft;
  char *endp;

  for (char *p = beg; p < lim; p = endp)
    {
      size_t match_size;
      size_t match_offset = ctx->execute (ctx, ctx->compiled_pattern, p, lim - p,
                                     &match_size, NULL);
      if (match_offset == (size_t) -1)
        {
          if (!out_invert)
            break;
          match_offset = lim - p;
          match_size = 0;
        }
      char *b = p + match_offset;
      endp = b + match_size;
      /* Avoid matching the empty line at the end of the buffer. */
      if (!out_invert && b == lim)
        break;
      if (!out_invert || p < b)
        {
          char *prbeg = out_invert ? p : b;
          char *prend = out_invert ? b : endp;
          prtext (ctx, prbeg, prend);
          if (!outleft || done_on_match)
            {
              break;
            }
        }
    }

  return outleft0 - outleft;
}

/* Search a given (non-directory) file.  Return a count of lines printed.
   Set *INEOF to true if end-of-file reached.  */
static bool
grep_run (struct grep_ctx *ctx)
{
    int fd = ctx->head->next->cur_fd;
    struct stat const *st = &ctx->st;
    char eol = ctx->options[NULL_BOUND] ? '\0' : '\n';
    char nul_zapper = '\0';
    ctx->done_on_match_0 = done_on_match;
    ctx->out_quiet_0 = out_quiet;

    for (bool firsttime = true; ; firsttime = false)
    {
        if (ctx->nlines_first_null < 0 && eol && binary_files != TEXT_BINARY_FILES
                && (buf_has_nulls (bufbeg, buflim - bufbeg)
                    || (firsttime && file_must_have_nulls (ctx, buflim - bufbeg, fd, st))))
        {
            if (binary_files == WITHOUT_MATCH_BINARY_FILES) {
                ctx->state = CLOSE;
                break;
            }
            if (!ctx->options[COUNT])
                done_on_match = out_quiet = true;
            ctx->nlines_first_null = ctx->nlines;
            nul_zapper = eol;
            skip_nuls = skip_empty_lines;
        }

        lastnl = bufbeg;
        if (lastout)
            lastout = bufbeg;

        ctx->beg = bufbeg + ctx->save;

        /* no more data to scan (eof) except for maybe a residue -> break */
        if (ctx->beg == buflim)
        {
            ctx->ineof = true;
            if (ctx->residue)
            {
                *buflim++ = eol;
                if (outleft)
                    ctx->nlines += grepbuf (ctx, bufbeg + ctx->save - ctx->residue, buflim);
                if (pending)
                    prpending (ctx, buflim);
            }
            // Jump out of the loop to the next stage!
            ctx->state = POSTPROCESS;
            break;
        }

        zap_nuls (ctx->beg, buflim, nul_zapper);

        /* Determine new residue (the length of an incomplete line at the end of
           the buffer, 0 means there is no incomplete last line).  */
        ctx->oldc = ctx->beg[-1];
        ctx->beg[-1] = eol;
        /* FIXME: use rawmemrchr if/when it exists, since we have ensured
           that this use of memrchr is guaranteed never to return NULL.  */
        ctx->lim = memrchr (ctx->beg - 1, eol, buflim - ctx->beg + 1);
        ++ctx->lim;
        ctx->beg[-1] = ctx->oldc;
        if (ctx->lim == ctx->beg)
            ctx->lim = ctx->beg - ctx->residue;
        ctx->beg -= ctx->residue;
        ctx->residue = buflim - ctx->lim;

        if (ctx->beg < ctx->lim)
        {
            if (outleft)
                ctx->nlines += grepbuf (ctx, ctx->beg, ctx->lim);
            if (pending)
                prpending (ctx, ctx->lim);
            if ((!outleft && !pending)
                    || (done_on_match && MAX (0, ctx->nlines_first_null) < ctx->nlines)) {
                // Jump out of the loop to the next stage!
                ctx->state = POSTPROCESS;
                break;
            }
        }

        /* The last OUT_BEFORE lines at the end of the buffer will be needed as
           leading context if there is a matching line at the begin of the
           next data. Make beg point to their begin.  */
        intmax_t i = 0;
        ctx->beg = ctx->lim;
        while (i < ctx->options[CONTEXT_BEFORE] && ctx->beg > bufbeg && ctx->beg != lastout)
        {
            ++i;
            do
                --ctx->beg;
            while (ctx->beg[-1] != eol);
        }

        /* Detect whether leading context is adjacent to previous output.  */
        if (ctx->beg != lastout)
            lastout = 0;

        /* Handle some details and read more data to scan.  */
        ctx->save = ctx->residue + ctx->lim - ctx->beg;
        if (out_byte)
            totalcc = add_count (totalcc, buflim - bufbeg - ctx->save);
        if (out_line)
            nlscan (ctx, ctx->beg);
        if (! fillbuf (ctx, ctx->save, st))
        {
            suppressible_error (ctx, EFAULT);
            // bail out because we faulted trying to fill the buffer.
            ctx->state = CLOSE;
            break;
        }
        if (slbuf_full(ctx->out)) {
            // Yield execution to the next call.
            break;
        } else {
            // Try yielding execution to another thread.
            maybe_yield();
        }
    }
    return 0;
}

/* True if errno is ERR after 'open ("symlink", ... O_NOFOLLOW ...)'.
   POSIX specifies ELOOP, but it's EMLINK on FreeBSD and EFTYPE on NetBSD.  */
static bool
open_symlink_nofollow_error (int err)
{
  if (err == ELOOP || err == EMLINK)
    return true;
#ifdef EFTYPE
  if (err == EFTYPE)
    return true;
#endif
  return false;
}

static bool
grep_open_file (struct grep_ctx *ctx)
{
    if (ctx->head->next == ctx->tail) {
        // Bail out because there is no work to be done.
        return 1;
    }
    int dirdesc = ctx->head->next->at_fd;
    char *name = ctx->head->next->name;
    bool follow = (dirdesc == AT_FDCWD);
    bool command_line = (dirdesc == AT_FDCWD);

    int oflag = (O_RDONLY | O_NOCTTY
            | (IGNORE_DUPLICATE_BRANCH_WARNING
                (binary ? O_BINARY : 0))
            | (follow ? 0 : O_NOFOLLOW)
            | (skip_devices (command_line) ? O_NONBLOCK : 0));

    if (kern_openat (ctx->td, dirdesc, name, UIO_SYSSPACE, oflag, 0644))
    {
        if (follow || ! open_symlink_nofollow_error (EFAULT))
            suppressible_error (ctx, EFAULT);
        // Jump to CLOSE to skip past this file.
        ctx->state = CLOSE;
        return true;
    } else {
        ctx->head->next->cur_fd =  ctx->td->td_retval[0];
    }
    // Move to PREPROCESSING to actually inspect this file.
    ctx->state = PREPROCESS;
    return false;
}

static bool
grep_close_file (struct grep_ctx *ctx) {
    match_rem_file (ctx, ctx->head->next);
    ctx->state = NEW;
    return 0;
}

/* Read all data from FD, with status ST.  Return true if successful,
   false (setting errno) otherwise.  */
static bool
drain_input (struct grep_ctx *ctx, int fd, struct stat const *st)
{
  ssize_t nbytes;
  if (S_ISFIFO (st->st_mode) && dev_null_output)
    {
#ifdef SPLICE_F_MOVE
      /* Should be faster, since it need not copy data to user space.  */
      nbytes = splice (fd, NULL, STDOUT_FILENO, NULL,
                       INITIAL_BUFSIZE, SPLICE_F_MOVE);
      if (0 <= nbytes || errno != EINVAL)
        {
          while (0 < nbytes)
            nbytes = splice (fd, NULL, STDOUT_FILENO, NULL,
                             INITIAL_BUFSIZE, SPLICE_F_MOVE);
          return nbytes == 0;
        }
#endif
    }
  do {
        struct uio auio;
        struct iovec aiov;

        aiov.iov_base = buffer;
        aiov.iov_len = bufalloc;
        auio.uio_iov = &aiov;
        auio.uio_iovcnt = 1;
        auio.uio_resid = bufalloc;
        auio.uio_segflg = UIO_SYSSPACE;

        if(kern_readv(ctx->td, fd, &auio)) {
            break;
        }
        nbytes = bufalloc - auio.uio_resid;
  } while (nbytes);
  return true;
}

/* Finish reading from FD, with status ST and where end-of-file has
   been seen if INEOF.  Typically this is a no-op, but when reading
   from standard input this may adjust the file offset or drain a
   pipe.  */

static void
finalize_input (struct grep_ctx *ctx)
{
    int fd = ctx->head->next->cur_fd;
    struct stat *st = &ctx->st;
  if (fd == STDIN_FILENO
      && (outleft
          ? (!ctx->ineof
             && (ctx->seek_failed
                 || (kern_lseek (ctx->td, fd, 0, SEEK_END) < 0
                     /* Linux proc file system has EINVAL (Bug#25180).  */
                     && ctx->td->td_errno != EINVAL))
             && ! drain_input (ctx, fd, st))
          : (bufoffset != after_last_match && !ctx->seek_failed
             && kern_lseek (ctx->td, fd, after_last_match, SEEK_SET) < 0)))
    suppressible_error (ctx, EFAULT);
}

static bool
grep_preprocess (struct grep_ctx *ctx)
{
    int desc = ctx->head->next->cur_fd;
    bool command_line = (ctx->head->next->at_fd == AT_FDCWD);

    /* Get the file status, possibly for the second time.  This catches
       a race condition if the directory entry changes after the
       directory entry is read and before the file is opened.  For
       example, normally DESC is a directory only at the top level, but
       there is an exception if some other process substitutes a
       directory for a non-directory while 'grep' is running.  */
    if (kern_fstat (ctx->td, desc, &(ctx->st)) != 0)
    {
        suppressible_error (ctx, EFAULT);
        ctx->state = CLOSE;
        return 0;
    }

    if (desc != STDIN_FILENO && skip_devices (command_line)
            && is_device_mode (ctx->st.st_mode)) {

        ctx->state = CLOSE;
        return 0;
    }

#if 0
    if (desc != STDIN_FILENO && command_line
            && skipped_file (filename, true, S_ISDIR (st.st_mode) != 0))
        goto closeout;
#endif

    if (desc != STDIN_FILENO
            && directories == RECURSE_DIRECTORIES && S_ISDIR (ctx->st.st_mode))
    {
        /* Traverse the directory starting with its full name, because
           unfortunately fts provides no way to traverse the directory
           starting from its file descriptor.  */

        ctx->state = CLOSE;
        return 0;
    }
    if (desc != STDIN_FILENO
            && ((directories == SKIP_DIRECTORIES && S_ISDIR (ctx->st.st_mode))
                || ((devices == SKIP_DEVICES
                        || (devices == READ_COMMAND_LINE_DEVICES && !command_line))
                    && is_device_mode (ctx->st.st_mode)))) {

        ctx->state = CLOSE;
        return 0;
    }

    /* If there is a regular file on stdout and the current file refers
       to the same i-node, we have to report the problem and skip it.
       Otherwise when matching lines from some other input reach the
       disk before we open this file, we can end up reading and matching
       those lines and appending them to the file from which we're reading.
       Then we'd have what appears to be an infinite loop that'd terminate
       only upon filling the output file system or reaching a quota.
       However, there is no risk of an infinite loop if grep is generating
       no output, i.e., with --silent, --quiet, -q.
       Similarly, with any of these:
       --max-count=N (-m) (for N >= 2)
       --files-with-matches (-l)
       --files-without-match (-L)
       there is no risk of trouble.
       For --max-count=1, grep stops after printing the first match,
       so there is no risk of malfunction.  But even --max-count=2, with
       input==output, while there is no risk of infloop, there is a race
       condition that could result in "alternate" output.  */
    if (!out_quiet && list_files == LISTFILES_NONE && 1 < ctx->options[MATCH_LIMIT]
            && S_ISREG (ctx->st.st_mode) && SAME_INODE (ctx->st, out_stat))
    {
        if (! ctx->options[SUPPRESS_ERRORS])
            error (0, 0, _("input file %s is also the output"),
                    quote (input_filename (ctx)));
        ctx->errseen = true;

        ctx->state = CLOSE;
        return 0;
    }

    // Fall through and initialize the matcher to start running.

    /* The value of NLINES when nulls were first deduced in the input;
       this is not necessarily the same as the number of matching lines
       before the first null.  -1 if no input nulls have been deduced.  */
    ctx->nlines_first_null = -1;

    int fd = ctx->head->next->cur_fd;
    struct stat const *st = &ctx->st;
    char eol = ctx->options[NULL_BOUND] ? '\0' : '\n';

    if (! reset (ctx, fd, st)) {
        ctx->state = CLOSE;
        return 0;
    }


    totalcc = 0;
    lastout = 0;
    totalnl = 0;
    outleft = ctx->options[MATCH_LIMIT];
    after_last_match = 0;
    pending = 0;
    skip_nuls = skip_empty_lines && !eol;
    ctx->encoding_error_output = false;

    ctx->nlines = 0;
    ctx->residue = 0;
    ctx->save = 0;

    if (! fillbuf (ctx, ctx->save, st))
    {
        suppressible_error (ctx, EFAULT);
        ctx->state = CLOSE;
        return 0;
    }

    ctx->offset_width = 0;
    if (ctx->options[TAB_ALIGN])
    {
        /* Width is log of maximum number.  Line numbers are origin-1.  */
        uintmax_t num = usable_st_size (st) ? st->st_size : UINTMAX_MAX;
        num += out_line && num < UINTMAX_MAX;
        do
            ctx->offset_width++;
        while ((num /= 10) != 0);
    }

    // We're ready to run!
    ctx->state = RUN_MATCH;
    return 0;
}

static bool
grep_postprocess (struct grep_ctx *ctx)
{

    done_on_match = ctx->done_on_match_0;
    out_quiet = ctx->out_quiet_0;
    if (!out_quiet && (ctx->encoding_error_output
                || (0 <= ctx->nlines_first_null && ctx->nlines_first_null < ctx->nlines)))
    {
        printf_errno (ctx, _("Binary file %s matches\n"), input_filename (ctx));
        if (line_buffered)
            fflush_errno ();
    }

    bool status;
    if (ctx->options[COUNT])
    {
        if (out_file)
        {
            print_filename (ctx);
            if (filename_mask)
                print_sep (ctx, SEP_CHAR_SELECTED);
            else
                putchar_errno (ctx, 0);
        }
        printf_errno (ctx, "%" PRIdMAX "\n", ctx->count);
        if (line_buffered)
            fflush_errno ();
    }

    status = !ctx->count == !(list_files == LISTFILES_NONMATCHING);

    if (list_files == LISTFILES_NONE)
        finalize_input (ctx);
    else if (status == 0)
    {
        print_filename (ctx);
        putchar_errno (ctx, '\n' & filename_mask);
        if (line_buffered)
            fflush_errno ();
    }

    ctx->state = CLOSE;
    return status;
}

/* Pattern compilers and matchers.  */

const matcher_t matchers[7] = {
  { "grep", RE_SYNTAX_GREP, GEAcompile, EGexecute },
  { "egrep", RE_SYNTAX_EGREP, GEAcompile, EGexecute },
  { "fgrep", 0, Fcompile, Fexecute, },
  { "awk", RE_SYNTAX_AWK, GEAcompile, EGexecute },
  { "gawk", RE_SYNTAX_GNU_AWK, GEAcompile, EGexecute },
  { "posixawk", RE_SYNTAX_POSIX_AWK, GEAcompile, EGexecute },
  { "perl", 0, Pcompile, Pexecute, },
};
/* Keep these in sync with the 'matchers' table.  */
enum { E_MATCHER_INDEX = 1, F_MATCHER_INDEX = 2, G_MATCHER_INDEX = 0 };

/* Parse GREP_COLORS.  The default would look like:
     GREP_COLORS='ms=01;31:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'
   with boolean capabilities (ne and rv) unset (i.e., omitted).
   No character escaping is needed or supported.  */
void
parse_grep_colors (struct grep_ctx *ctx, const char *p)
{
  char *q;
  char *name;
  char *val;

  if (p == NULL || *p == '\0')
    return;

  /* Work off a writable copy.  */
  q = xstrdup (p);

  name = q;
  val = NULL;
  /* From now on, be well-formed or you're gone.  */
  for (;;)
    if (*q == ':' || *q == '\0')
      {
        char c = *q;
        struct color_cap const *cap;

        *q++ = '\0'; /* Terminate name or val.  */
        /* Empty name without val (empty cap)
         * won't match and will be ignored.  */
        for (cap = ctx->color_dict; cap->name; cap++)
          if (STREQ (cap->name, name))
            break;
        /* If name unknown, go on for forward compatibility.  */
        if (cap->var && val)
          *(cap->var) = val;
        if (cap->fct)
          cap->fct (ctx);
        if (c == '\0')
          return;
        name = q;
        val = NULL;
      }
    else if (*q == '=')
      {
        if (q == name || val)
          return;
        *q++ = '\0'; /* Terminate name.  */
        val = q; /* Can be the empty string.  */
      }
    else if (val == NULL)
      q++; /* Accumulate name.  */
    else if (*q == ';' || c_isdigit (*q))
      q++; /* Accumulate val.  Protect the terminal from being sent crap.  */
    else
      return;
}

/* Change the pattern *KEYS_P, of size *LEN_P, from fgrep to grep style.  */

void
fgrep_to_grep_pattern (struct grep_ctx *ctx, char **keys_p, size_t *len_p)
{
  size_t len = *len_p;
  char *keys = *keys_p;
  mbstate_t mb_state = { {0} };
  char *new_keys = xnmalloc (len + 1, 2);
  char *p = new_keys;
  size_t n;

  for (; len; keys += n, len -= n)
    {
      n = mb_clen (&ctx->localeinfo, keys, len, &mb_state);
      switch (n)
        {
        case (size_t) -2:
          n = len;
          FALLTHROUGH;
        default:
          memcpy (p, keys, n);
          p += n;
          break;

        case (size_t) -1:
          memset (&mb_state, 0, sizeof mb_state);
          n = 1;
          FALLTHROUGH;
        case 1:
          switch (*keys)
            {
            case '$': case '*': case '.': case '[': case '\\': case '^':
              *p++ = '\\'; break;
            }
          *p++ = *keys;
          break;
        }
    }

  free (*keys_p);
  *keys_p = new_keys;
  *len_p = p - new_keys;
}

bool
grep_reenter(struct grep_ctx *ctx) {
    switch (ctx->state) {
        case NEW:
            return grep_open_file(ctx);
        case PREPROCESS:
            return grep_preprocess(ctx);
        case RUN_MATCH:
            return grep_run(ctx);
        case POSTPROCESS:
            return grep_postprocess(ctx);
        case CLOSE:
            return grep_close_file(ctx);
    }
}

void init_globals(struct grep_ctx *ctx) {
  /* Prefer sysconf for page size, as getpagesize typically returns int.  */
  long psize = 4096;
  if (! (0 < psize && psize <= (SIZE_MAX - sizeof (uword)) / 2))
    abort ();
  pagesize = psize;
  bufalloc = ALIGN_TO (INITIAL_BUFSIZE, pagesize) + pagesize + sizeof (uword);
  buffer = xmalloc (bufalloc);
  memset(buffer, 0, bufalloc);

  initialize_unibyte_mask (ctx);
  init_localeinfo (&ctx->localeinfo);
  devices = READ_DEVICES;
  binary_files = TEXT_BINARY_FILES;
}

void clean_globals(struct grep_ctx *ctx) {
    free(buffer);
}

