/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions to process command line options.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#define _GNU_SOURCE   /* for asprintf */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>  /* to declare isspace () */

#if defined(HAVE_SCANDIR)
#include <dirent.h>
#endif

#include "ctags.h"
#include "debug.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parse.h"
#include "routines.h"

/*
*   MACROS
*/
#define INVOCATION  "Usage: %s [options] [file(s)]\n"

#define CTAGS_DATA_PATH_ENVIRONMENT "CTAGS_DATA_PATH"
#define CTAGS_LIBEXEC_PATH_ENVIRONMENT "CTAGS_LIBEXEC_PATH"
#define CTAGS_ENVIRONMENT  "CTAGS"
#define ETAGS_ENVIRONMENT  "ETAGS"

#define CTAGS_FILE  "tags"
#define ETAGS_FILE  "TAGS"

#ifndef ETAGS
# define ETAGS	"etags"  /* name which causes default use of to -e */
#endif

/*  The following separators are permitted for list options.
 */
#define EXTENSION_SEPARATOR '.'
#define PATTERN_START '('
#define PATTERN_STOP  ')'
#define IGNORE_SEPARATORS   ", \t\n"

#ifndef DEFAULT_FILE_FORMAT
# define DEFAULT_FILE_FORMAT  2
#endif

#if defined (HAVE_OPENDIR) || defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST) || defined (AMIGA)
# define RECURSE_SUPPORTED
#endif

#define isCompoundOption(c)  (boolean) (strchr ("fohiILpDb", (c)) != NULL)

#define SUBDIR_OPTLIB "optlib"
#define SUBDIR_CORPORA "corpora"
#define SUBDIR_PRELOAD "preload"
#define SUBDIR_DRIVERS "drivers"

/*
*   Data declarations
*/

enum eOptionLimits {
	MaxHeaderExtensions	= 100,  /* maximum number of extensions in -h option */
	MaxSupportedTagFormat = 2
};

typedef struct sOptionDescription {
	int usedByEtags;
	const char *description;
} optionDescription;

typedef void (*parametricOptionHandler) (const char *const option, const char *const parameter);

typedef const struct {
	const char* name;   /* name of option as specified by user */
	parametricOptionHandler handler;  /* routine to handle option */
	boolean initOnly;   /* option must be specified before any files */
} parametricOption;

typedef const struct {
	const char* name;   /* name of option as specified by user */
	boolean* pValue;    /* pointer to option value */
	boolean initOnly;   /* option must be specified before any files */
} booleanOption;

/*
*   DATA DEFINITIONS
*/

static boolean NonOptionEncountered;
static stringList *OptionFiles;

typedef stringList searchPathList;
static searchPathList *OptlibPathList;
static searchPathList *CorpusPathList;
static searchPathList *PreloadPathList;
static searchPathList *DriversPathList;

static stringList* Excluded;
static boolean FilesRequired = TRUE;
static boolean SkipConfiguration;

static const char *const HeaderExtensions [] = {
	"h", "H", "hh", "hpp", "hxx", "h++", "inc", "def", NULL
};

optionValues Option = {
	{
		FALSE,  /* --extra=f */
		FALSE,  /* --extra=q */
		TRUE,   /* --file-scope */
	},
	{
		FALSE,  /* -fields=a */
		TRUE,   /* -fields=f */
		FALSE,  /* -fields=m */
		FALSE,  /* -fields=i */
		TRUE,   /* -fields=k */
		FALSE,  /* -fields=z */
		FALSE,  /* -fields=K */
		FALSE,  /* -fields=l */
		FALSE,  /* -fields=n */
		TRUE,   /* -fields=s */
		FALSE,  /* -fields=S */
		TRUE    /* -fields=t */
	},
	NULL,       /* -I */
	FALSE,      /* -a */
	FALSE,      /* -B */
	FALSE,      /* -e */
#ifdef MACROS_USE_PATTERNS
	EX_PATTERN, /* -n, --excmd */
#else
	EX_MIX,     /* -n, --excmd */
#endif
	FALSE,      /* -R */
	SO_SORTED,  /* -u, --sort */
	FALSE,      /* -V */
	FALSE,      /* -x */
	NULL,       /* -L */
	NULL,       /* -o */
	NULL,       /* -h */
	NULL,		/* --config-filename */
	NULL,       /* --etags-include */
	DEFAULT_FILE_FORMAT,/* --format */
	FALSE,      /* --if0 */
	FALSE,      /* --kind-long */
	LANG_AUTO,  /* --lang */
	TRUE,       /* --links */
	FALSE,      /* --filter */
	NULL,       /* --filter-terminator */
	FALSE,      /* --tag-relative */
	FALSE,      /* --totals */
	FALSE,      /* --line-directives */
	FALSE,	    /* --print-language */
	FALSE,	    /* --guess-language-eagerly(-G) */
#ifdef DEBUG
	0, 0        /* -D, -b */
#endif
};

/*
-   Locally used only
*/

static optionDescription LongOptionDescription [] = {
 {1,"  -a   Append the tags to an existing tag file."},
#ifdef DEBUG
 {1,"  -b <line>"},
 {1,"       Set break line."},
#endif
 {0,"  -B   Use backward searching patterns (?...?)."},
#ifdef DEBUG
 {1,"  -D <level>"},
 {1,"       Set debug level."},
#endif
 {0,"  -e   Output tag file for use with Emacs."},
 {1,"  -f <name>"},
 {1,"       Write tags to specified file. Value of \"-\" writes tags to stdout"},
 {1,"       [\"tags\"; or \"TAGS\" when -e supplied]."},
 {0,"  -F   Use forward searching patterns (/.../) (default)."},
 {1,"  -G   Equivalent to --guess-language-eagerly."},
 {1,"  -h <list>"},
 {1,"       Specify list of file extensions to be treated as include files."},
 {1,"       [\".h.H.hh.hpp.hxx.h++\"]."},
 {1,"  -I <list|@file>"},
 {1,"       A list of tokens to be specially handled is read from either the"},
 {1,"       command line or the specified file."},
 {1,"  -L <file>"},
 {1,"       A list of source file names are read from the specified file."},
 {1,"       If specified as \"-\", then standard input is read."},
 {0,"  -n   Equivalent to --excmd=number."},
 {0,"  -N   Equivalent to --excmd=pattern."},
 {1,"  -o   Alternative for -f."},
#ifdef RECURSE_SUPPORTED
 {1,"  -R   Equivalent to --recurse."},
#else
 {1,"  -R   Not supported on this platform."},
#endif
 {0,"  -u   Equivalent to --sort=no."},
 {1,"  -V   Equivalent to --verbose."},
 {1,"  -x   Print a tabular cross reference file to standard output."},
 {1,"  --append=[yes|no]"},
 {1,"       Should tags should be appended to existing tag file [no]?"},
 {1,"  --config-filename=fileName"},
 {1,"      Use 'fileName' instead of 'ctags' in option file names."},
 {1,"  --data-dir=[+]DIR"},
 {1,"      Add or set DIR to data directory search path."},
 {1,"  --etags-include=file"},
 {1,"      Include reference to 'file' in Emacs-style tag file (requires -e)."},
 {1,"  --exclude=pattern"},
 {1,"      Exclude files and directories matching 'pattern'."},
 {0,"  --excmd=number|pattern|mix"},
#ifdef MACROS_USE_PATTERNS
 {0,"       Uses the specified type of EX command to locate tags [pattern]."},
#else
 {0,"       Uses the specified type of EX command to locate tags [mix]."},
#endif
 {1,"  --extra=[+|-]flags"},
 {1,"      Include extra tag entries for selected information (flags: \"fq\")."},
 {1,"  --fields=[+|-]flags"},
 {1,"      Include selected extension fields (flags: \"afmikKlnsStz\") [fks]."},
 {1,"  --file-scope=[yes|no]"},
 {1,"       Should tags scoped only for a single file (e.g. \"static\" tags"},
 {1,"       be included in the output [yes]?"},
 {1,"  --filter=[yes|no]"},
 {1,"       Behave as a filter, reading file names from standard input and"},
 {1,"       writing tags to standard output [no]."},
 {1,"  --filter-terminator=string"},
 {1,"       Specify string to print to stdout following the tags for each file"},
 {1,"       parsed when --filter is enabled."},
 {1,"  --guess-language-eagerly"},
 {1,"       Guess the language of input file more eagerly: "},
 {1,"       (but taking longer time for guessing)"},
 {1,"       o shebang even the input file is not executable,"},
 {1,"       o emacs mode specification at the beginning and end of input file, and"},
 {1,"       o vim syntax specification at the end of input file."},
 {0,"  --format=level"},
#if DEFAULT_FILE_FORMAT == 1
 {0,"       Force output of specified tag file format [1]."},
#else
 {0,"       Force output of specified tag file format [2]."},
#endif
 {1,"  --help"},
 {1,"       Print this option summary."},
 {1,"  --if0=[yes|no]"},
 {1,"       Should C code within #if 0 conditional branches be parsed [no]?"},
 {1,"  --<LANG>-alias=[+|-]aliasPattern"},
 {1,"      Add a pattern detecting a name can be used as an alternative name for LANG."},
 {1,"  --<LANG>-corpus=spec:corpusFile"},
 {1,"      Add two gram data calculated from corpusFile to spec of LANG."},
 {1,"  --<LANG>-kinds=[+|-]kinds"},
 {1,"       Enable/disable tag kinds for language <LANG>."},
 {1,"  --<LANG>-map=[+]map"},
 {1,"       Simplified version of --langmap opton."},
#ifdef HAVE_REGEX
 {1,"  --<LANG>-regex=/line_pattern/name_pattern/[flags]"},
 {1,"       Same as --regex-<LANG>=..."},
#endif
#ifdef HAVE_COPROC
 {1,"  --<LANG>-xcmd=parser_command_path|parset_command_name"},
 {1,"       Same as --xcmd-<LANG>=..."},
#endif
 {1,"  --langdef=name"},
 {1,"       Define a new language to be parsed with regular expressions."},
 {1,"  --langmap=map(s)"},
 {1,"       Override default mapping of language to source file extension."},
 {1,"  --language-force=language"},
 {1,"       Force all files to be interpreted using specified language."},
 {1,"  --languages=[+|-]list"},
 {1,"       Restrict files scanned for tags to those mapped to langauges"},
 {1,"       specified in the comma-separated 'list'. The list can contain any"},
 {1,"       built-in or user-defined language [all]."},
 {1,"  --libexec-dir=[+]DIR"},
 {1,"      Add or set DIR to libexec directory search path."},
 {1,"  --license"},
 {1,"       Print details of software license."},
 {0,"  --line-directives=[yes|no]"},
 {0,"       Should #line directives be processed [no]?"},
 {1,"  --links=[yes|no]"},
 {1,"       Indicate whether symbolic links should be followed [yes]."},
 {1,"  --list-aliases=[language|all]"},
 {1,"       Output list of alias patterns."},
 {1,"  --list-corpora=[language[:spec]|all]"},
 {1,"       Output list of language corpora."},
 {1,"  --list-features"},
 {1,"       Output list of features."},
 {1,"  --list-kinds=[language|all]"},
 {1,"       Output a list of all tag kinds for specified language or all."},
 {1,"  --list-languages"},
 {1,"       Output list of supported languages."},
 {1,"  --list-maps=[language|all]"},
 {1,"       Output list of language mappings."},
 {1,"  --options=file"},
 {1,"       Specify file from which command line options should be read."},
 {0,"  --print-language"},
 {0,"       Don't make tags file but just print the guessed language name for input file."},
 {1,"  --recurse=[yes|no]"},
#ifdef RECURSE_SUPPORTED
 {1,"       Recurse into directories supplied on command line [no]."},
#else
 {1,"       Not supported on this platform."},
#endif
#ifdef HAVE_REGEX
 {1,"  --regex-<LANG>=/line_pattern/name_pattern/[flags]"},
 {1,"       Define regular expression for locating tags in specific language."},
#endif
 {0,"  --sort=[yes|no|foldcase]"},
 {0,"       Should tags be sorted (optionally ignoring case) [yes]?."},
 {0,"  --tag-relative=[yes|no]"},
 {0,"       Should paths be relative to location of tag file [no; yes when -e]?"},
 {1,"  --totals=[yes|no]"},
 {1,"       Print statistics about source and tag files [no]."},
 {1,"  --verbose=[yes|no]"},
 {1,"       Enable verbose messages describing actions on each source file."},
 {1,"  --version"},
 {1,"       Print version identifier to standard output."},
#ifdef HAVE_COPROC
 {1,"  --xcmd-<LANG>=parser_command_path|parset_command_name"},
 {1,"       Define external parser command path or name for specific language."},
#endif
 {1, NULL}
};

static const char* const License1 =
"This program is free software; you can redistribute it and/or\n"
"modify it under the terms of the GNU General Public License\n"
"as published by the Free Software Foundation; either version 2\n"
"of the License, or (at your option) any later version.\n"
"\n";
static const char* const License2 =
"This program is distributed in the hope that it will be useful,\n"
"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"GNU General Public License for more details.\n"
"\n"
"You should have received a copy of the GNU General Public License\n"
"along with this program; if not, write to the Free Software\n"
"Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n";

/*  Contains a set of strings describing the set of "features" compiled into
 *  the code.
 */
static const char *const Features [] = {
#ifdef WIN32
	"win32",
#endif
#ifdef DJGPP
	"msdos_32",
#else
# ifdef MSDOS
	"msdos_16",
# endif
#endif
#ifdef OS2
	"os2",
#endif
#ifdef AMIGA
	"amiga",
#endif
#ifdef VMS
	"vms",
#endif
#ifdef HAVE_FNMATCH
	"wildcards",
#endif
#ifdef HAVE_REGEX
	"regex",
#endif
#ifndef EXTERNAL_SORT
	"internal-sort",
#endif
#ifdef CUSTOM_CONFIGURATION_FILE
	"custom-conf",
#endif
#if (defined (MSDOS) || defined (WIN32) || defined (OS2)) && defined (UNIX_PATH_SEPARATOR)
	"unix-path-separator",
#endif
#ifdef DEBUG
	"debug",
#endif
#ifdef HAVE_SCANDIR
	"option-directory",
#endif
#ifdef HAVE_COPROC
	"coproc",
#endif
	NULL
};

/*
*   FUNCTION PROTOTYPES
*/
static boolean parseFileOptions (const char *const fileName);
static boolean parseAllConfigurationFilesOptionsInDirectory (const char *const fileName,
							     stringList* const already_loaded_files);

/*
*   FUNCTION DEFINITIONS
*/


static vString* getHome (void)
{
	const char* const home = getenv ("HOME");

	if (home)
		return vStringNewInit (home);
	else
	{
#ifdef MSDOS_STYLE_PATH
		/*
		 * Windows users don't usually set HOME.
		 * The OS sets HOMEDRIVE and HOMEPATH for them.
		 */
		const char* homeDrive = getenv ("HOMEDRIVE");
		const char* homePath = getenv ("HOMEPATH");
		if (homeDrive != NULL && homePath != NULL)
		{
			vString* const windowsHome = vStringNew ();
			vStringCatS (windowsHome, homeDrive);
			vStringCatS (windowsHome, homePath);
			return windowsHome;
		}
#endif
		return NULL;
	}
}

#if defined(_WIN32)

/* Some versions of MinGW are missing _vscprintf's declaration, although they
 * still provide the symbol in the import library.
 */
#ifdef __MINGW32__
_CRTIMP int _vscprintf(const char *format, va_list argptr);
#endif

#ifndef va_copy
#define va_copy(dest, src) (dest = src)
#endif

int asprintf(char **strp, const char *fmt, ...)
{
	va_list args;
	va_list args_copy;
	int length;
	size_t size;

	va_start(args, fmt);

	va_copy(args_copy, args);

#ifdef _WIN32
	/* We need to use _vcsprintf to calculate the length as vsnprintf returns -1
	 * if the number of characters to write is greater than count.
	 */
	length = _vscprintf(fmt, args_copy);
#else
	char dummy;
	length = vsnprintf(&dummy, sizeof dummy, fmt, args_copy);
#endif

	va_end(args_copy);

	Assert(length >= 0);
	size = length + 1;

	*strp = malloc(size);
	if (!*strp) {
		return -1;
	}

	va_start(args, fmt);
	vsnprintf(*strp, size, fmt, args);
	va_end(args);

	return length;
}
#endif


#if (defined (__SVR4) && defined (__sun))
int vasprintf(char **ret, const char *format, va_list args)
{
	va_list copy;
	va_copy(copy, args);

	/* Make sure it is determinate, despite manuals indicating otherwise */
	*ret = 0;

	int count = vsnprintf(NULL, 0, format, args);
	if (count >= 0) {
		char* buffer = malloc(count + 1);
		if (buffer != NULL) {
			count = vsnprintf(buffer, count + 1, format, copy);
			if (count < 0)
				free(buffer);
			else
				*ret = buffer;
		}
	}
	va_end(args);  // Each va_start() or va_copy() needs a va_end()

	return count;
}

int asprintf(char **strp, const char *fmt, ...)
{
	s32 size;
	va_list args;
	va_start(args, fmt);
	size = vasprintf(strp, fmt, args);
	va_end(args);
	return size;
}
#endif

extern void verbose (const char *const format, ...)
{
	if (Option.verbose)
	{
		va_list ap;
		va_start (ap, format);
		vfprintf (stderr, format, ap);
		va_end (ap);
	}
}

static char *stringCopy (const char *const string)
{
	char* result = NULL;
	if (string != NULL)
		result = eStrdup (string);
	return result;
}

static void freeString (char **const pString)
{
	if (*pString != NULL)
	{
		eFree (*pString);
		*pString = NULL;
	}
}

extern void freeList (stringList** const pList)
{
	if (*pList != NULL)
	{
		stringListDelete (*pList);
		*pList = NULL;
	}
}

extern void setDefaultTagFileName (void)
{
	if (Option.tagFileName != NULL)
		;  /* accept given name */
	else if (Option.etags)
		Option.tagFileName = stringCopy (ETAGS_FILE);
	else
		Option.tagFileName = stringCopy (CTAGS_FILE);
}

extern boolean filesRequired (void)
{
	boolean result = FilesRequired;
	if (Option.recurse)
		result = FALSE;
	return result;
}

extern void checkOptions (void)
{
	const char* notice;
	if (Option.xref)
	{
		notice = "xref output";
		if (Option.include.fileNames)
		{
			error (WARNING, "%s disables file name tags", notice);
			Option.include.fileNames = FALSE;
		}
	}
	if (Option.append)
	{
		notice = "append mode is not compatible with";
		if (isDestinationStdout ())
			error (FATAL, "%s tags to stdout", notice);
	}
	if (Option.filter)
	{
		notice = "filter mode";
		if (Option.printTotals)
		{
			error (WARNING, "%s disables totals", notice);
			Option.printTotals = FALSE;
		}
		if (Option.tagFileName != NULL)
			error (WARNING, "%s ignores output tag file name", notice);
	}
}

static void setEtagsMode (void)
{
	Option.etags = TRUE;
	Option.sorted = SO_UNSORTED;
	Option.lineDirectives = FALSE;
	Option.tagRelative = TRUE;
}

extern void testEtagsInvocation (void)
{
	char* const execName = eStrdup (getExecutableName ());
	char* const etags = eStrdup (ETAGS);
#ifdef CASE_INSENSITIVE_FILENAMES
	toLowerString (execName);
	toLowerString (etags);
#endif
	if (strstr (execName, etags) != NULL)
	{
		verbose ("Running in etags mode\n");
		setEtagsMode ();
	}
	eFree (execName);
	eFree (etags);
}

/*
 *  Cooked argument parsing
 */

static void parseShortOption (cookedArgs *const args)
{
	args->simple [0] = *args->shortOptions++;
	args->simple [1] = '\0';
	args->item = args->simple;
	if (! isCompoundOption (*args->simple))
		args->parameter = "";
	else if (*args->shortOptions == '\0')
	{
		argForth (args->args);
		if (argOff (args->args))
			args->parameter = NULL;
		else
			args->parameter = argItem (args->args);
		args->shortOptions = NULL;
	}
	else
	{
		args->parameter = args->shortOptions;
		args->shortOptions = NULL;
	}
}

static void parseLongOption (cookedArgs *const args, const char *item)
{
	const char* const equal = strchr (item, '=');
	if (equal == NULL)
	{
		args->item = eStrdup (item);
		trashBoxPut (args->trash_box, args->item, (TrashBoxDestroyItemProc)eFree);
		args->parameter = "";
	}
	else
	{
		const size_t length = equal - item;
		args->item = eStrndup (item, length);
		trashBoxPut (args->trash_box, args->item, (TrashBoxDestroyItemProc)eFree);
		args->parameter = equal + 1;
	}
	Assert (args->item != NULL);
	Assert (args->parameter != NULL);
}

static void cArgRead (cookedArgs *const current)
{
	char* item;

	Assert (current != NULL);
	if (! argOff (current->args))
	{
		item = argItem (current->args);
		current->shortOptions = NULL;
		Assert (item != NULL);
		if (strncmp (item, "--", (size_t) 2) == 0)
		{
			current->isOption = TRUE;
			current->longOption = TRUE;
			parseLongOption (current, item + 2);
			Assert (current->item != NULL);
			Assert (current->parameter != NULL);
		}
		else if (*item == '-')
		{
			current->isOption = TRUE;
			current->longOption = FALSE;
			current->shortOptions = item + 1;
			parseShortOption (current);
		}
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->item = item;
			current->parameter = NULL;
		}
	}
}

extern cookedArgs* cArgNewFromString (const char* string)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromString (string);
	result->trash_box = trashBoxNew ();
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromArgv (char* const* const argv)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromArgv (argv);
	result->trash_box = trashBoxNew ();
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromFile (fp);
	result->trash_box = trashBoxNew ();
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromLineFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = argNewFromLineFile (fp);
	result->trash_box = trashBoxNew ();
	cArgRead (result);
	return result;
}

extern void cArgDelete (cookedArgs* const current)
{
	Assert (current != NULL);
	argDelete (current->args);
	trashBoxDelete (current->trash_box);
	memset (current, 0, sizeof (cookedArgs));
	eFree (current);
}

static boolean cArgOptionPending (cookedArgs* const current)
{
	boolean result = FALSE;
	if (current->shortOptions != NULL)
		if (*current->shortOptions != '\0')
			result = TRUE;
	return result;
}

extern boolean cArgOff (cookedArgs* const current)
{
	Assert (current != NULL);
	return (boolean) (argOff (current->args) && ! cArgOptionPending (current));
}

extern boolean cArgIsOption (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->isOption;
}

extern const char* cArgItem (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->item;
}

extern void cArgForth (cookedArgs* const current)
{
	Assert (current != NULL);
	Assert (! cArgOff (current));
	if (cArgOptionPending (current))
		parseShortOption (current);
	else
	{
		Assert (! argOff (current->args));
		argForth (current->args);
		if (! argOff (current->args))
			cArgRead (current);
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->shortOptions = NULL;
			current->item = NULL;
			current->parameter = NULL;
		}
	}
}

/*
 *  File extension and language mapping
 */

static void addExtensionList (
		stringList *const slist, const char *const elist, const boolean clear)
{
	char *const extensionList = eStrdup (elist);
	const char *extension = NULL;
	boolean first = TRUE;

	if (clear)
	{
		verbose ("      clearing\n");
		stringListClear (slist);
	}
	verbose ("      adding: ");
	if (elist != NULL  &&  *elist != '\0')
	{
		extension = extensionList;
		if (elist [0] == EXTENSION_SEPARATOR)
			++extension;
	}
	while (extension != NULL)
	{
		char *separator = strchr (extension, EXTENSION_SEPARATOR);
		if (separator != NULL)
			*separator = '\0';
		verbose ("%s%s", first ? "" : ", ",
				*extension == '\0' ? "(NONE)" : extension);
		stringListAdd (slist, vStringNewInit (extension));
		first = FALSE;
		if (separator == NULL)
			extension = NULL;
		else
			extension = separator + 1;
	}
	if (Option.verbose)
	{
		fprintf (stderr, "\n      now: ");
		stringListPrint (slist, stderr);
		putc ('\n', stderr);
	}
	eFree (extensionList);
}

static boolean isFalse (const char *parameter)
{
	return (boolean) (
		strcasecmp (parameter, "0"  ) == 0  ||
		strcasecmp (parameter, "n"  ) == 0  ||
		strcasecmp (parameter, "no" ) == 0  ||
		strcasecmp (parameter, "off") == 0);
}

static boolean isTrue (const char *parameter)
{
	return (boolean) (
		strcasecmp (parameter, "1"  ) == 0  ||
		strcasecmp (parameter, "y"  ) == 0  ||
		strcasecmp (parameter, "yes") == 0  ||
		strcasecmp (parameter, "on" ) == 0);
}

/*  Determines whether the specified file name is considered to be a header
 *  file for the purposes of determining whether enclosed tags are global or
 *  static.
 */
extern boolean isIncludeFile (const char *const fileName)
{
	boolean result = FALSE;
	const char *const extension = fileExtension (fileName);
	if (Option.headerExt != NULL)
		result = stringListExtensionMatched (Option.headerExt, extension);
	return result;
}

/*
 *  Specific option processing
 */

 static void processConfigFilenameOption (
 		const char *const option __unused__, const char *const parameter)
 {
 	freeString (&Option.configFilename);
 	Option.configFilename = stringCopy (parameter);
 }

static void processEtagsInclude (
		const char *const option, const char *const parameter)
{
	if (! Option.etags)
		error (FATAL, "Etags must be enabled to use \"%s\" option", option);
	else
	{
		vString *const file = vStringNewInit (parameter);
		if (Option.etagsInclude == NULL)
			Option.etagsInclude = stringListNew ();
		stringListAdd (Option.etagsInclude, file);
		FilesRequired = FALSE;
	}
}

static void processExcludeOption (
		const char *const option __unused__, const char *const parameter)
{
	const char *const fileName = parameter + 1;
	if (parameter [0] == '\0')
		freeList (&Excluded);
	else if (parameter [0] == '@')
	{
		stringList* const sl = stringListNewFromFile (fileName);
		if (sl == NULL)
			error (FATAL | PERROR, "cannot open \"%s\"", fileName);
		if (Excluded == NULL)
			Excluded = sl;
		else
			stringListCombine (Excluded, sl);
		verbose ("    adding exclude patterns from %s\n", fileName);
	}
	else
	{
		vString *const item = vStringNewInit (parameter);
		if (Excluded == NULL)
			Excluded = stringListNew ();
		stringListAdd (Excluded, item);
		verbose ("    adding exclude pattern: %s\n", parameter);
	}
}

extern boolean isExcludedFile (const char* const name)
{
	const char* base = baseFilename (name);
	boolean result = FALSE;
	if (Excluded != NULL)
	{
		result = stringListFileMatched (Excluded, base);
		if (! result  &&  name != base)
			result = stringListFileMatched (Excluded, name);
	}
#ifdef AMIGA
	/* not a good solution, but the only one which works often */
	if (! result)
		result = (boolean) (strcmp (name, TagFile.name) == 0);
#endif
	return result;
}

static void processExcmdOption (
		const char *const option, const char *const parameter)
{
	switch (*parameter)
	{
		case 'm': Option.locate = EX_MIX;     break;
		case 'n': Option.locate = EX_LINENUM; break;
		case 'p': Option.locate = EX_PATTERN; break;
		default:
			error (FATAL, "Invalid value for \"%s\" option", option);
			break;
	}
}

static void processExtraTagsOption (
		const char *const option, const char *const parameter)
{
	struct sInclude *const inc = &Option.include;
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	if (*p != '+'  &&  *p != '-')
	{
		inc->fileNames     = FALSE;
		inc->qualifiedTags = FALSE;
#if 0
		inc->fileScope     = FALSE;
#endif
	}
	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;                break;
		case '-': mode = FALSE;               break;

		case 'f': inc->fileNames     = mode;  break;
		case 'q': inc->qualifiedTags = mode;  break;
#if 0
		case 'F': inc->fileScope     = mode;  break;
#endif

		default: error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					   c, option);
			break;
	}
}

static void resetFieldsOption (
		struct sExtFields *field, boolean mode)
{
	memset(field, mode? ~0: 0, sizeof(*field));
}

static void processFieldsOption (
		const char *const option, const char *const parameter)
{
	struct sExtFields *field = &Option.extensionFields;
	const char *p = parameter;
	boolean mode = TRUE;
	int c;


	if (*p == '*')
	{
		resetFieldsOption(field, TRUE);
		p++;
	}
	else if (*p != '+'  &&  *p != '-')
		resetFieldsOption(field, FALSE);


	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;                  break;
		case '-': mode = FALSE;                 break;

		case 'a': field->access         = mode; break;
		case 'f': field->fileScope      = mode; break;
		case 'm': field->implementation = mode; break;
		case 'i': field->inheritance    = mode; break;
		case 'k': field->kind           = mode; break;
		case 'K': field->kindLong       = mode; break;
		case 'l': field->language       = mode; break;
		case 'n': field->lineNumber     = mode; break;
		case 's': field->scope          = mode; break;
		case 'S': field->signature      = mode; break;
		case 'z': field->kindKey        = mode; break;
		case 't': field->typeRef        = mode; break;

		default: error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					c, option);
			break;
	}
}

static void processFilterTerminatorOption (
		const char *const option __unused__, const char *const parameter)
{
	freeString (&Option.filterTerminator);
	Option.filterTerminator = stringCopy (parameter);
}

static void processFormatOption (
		const char *const option, const char *const parameter)
{
	unsigned int format;

	if (sscanf (parameter, "%u", &format) < 1)
		error (FATAL, "Invalid value for \"%s\" option",option);
	else if (format <= (unsigned int) MaxSupportedTagFormat)
		Option.tagFileFormat = format;
	else
		error (FATAL, "Unsupported value for \"%s\" option", option);
}

static void printInvocationDescription (void)
{
	printf (INVOCATION, getExecutableName ());
}

static void printOptionDescriptions (const optionDescription *const optDesc)
{
	int i;
	for (i = 0 ; optDesc [i].description != NULL ; ++i)
	{
		if (! Option.etags || optDesc [i].usedByEtags)
			puts (optDesc [i].description);
	}
}

static void printFeatureList (void)
{
	int i;

	for (i = 0 ; Features [i] != NULL ; ++i)
	{
		if (i == 0)
			printf ("  Optional compiled features: ");
		if (strcmp (Features [i], "regex") != 0 || checkRegex ())
			printf ("%s+%s", (i>0 ? ", " : ""), Features [i]);
#ifdef CUSTOM_CONFIGURATION_FILE
		if (strcmp (Features [i], "custom-conf") == 0)
			printf ("=%s", CUSTOM_CONFIGURATION_FILE);
#endif
	}
	if (i > 0)
		putchar ('\n');
}


static void processListFeaturesOption(const char *const option __unused__,
				      const char *const parameter __unused__)
{
	int i;

	for (i = 0 ; Features [i] != NULL ; ++i)
		if (strcmp (Features [i], "regex") != 0 || checkRegex ())
			printf ("%s\n", Features [i]);
	if (i == 0)
		putchar ('\n');
	exit (0);
}

static void printProgramIdentification (void)
{
	printf ("%s %s, %s %s\n",
	        PROGRAM_NAME, PROGRAM_VERSION,
	        PROGRAM_COPYRIGHT, AUTHOR_NAME);
	printf ("  Compiled: %s, %s\n", __DATE__, __TIME__);
	printf ("  Addresses: <%s>, %s\n", AUTHOR_EMAIL, PROGRAM_URL);
	printFeatureList ();
}

static void processHelpOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	putchar ('\n');
	printInvocationDescription ();
	putchar ('\n');
	printOptionDescriptions (LongOptionDescription);
	exit (0);
}

static void processLanguageForceOption (
		const char *const option, const char *const parameter)
{
	langType language;
	if (strcasecmp (parameter, "auto") == 0)
		language = LANG_AUTO;
	else
		language = getNamedLanguage (parameter);

	if (strcmp (option, "lang") == 0  ||  strcmp (option, "language") == 0)
		error (WARNING,
			   "\"--%s\" option is obsolete; use \"--language-force\" instead",
			   option);
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
	else
		Option.language = language;
}
static char* skipPastMap (char* p)
{
	while (*p != EXTENSION_SEPARATOR  &&
			*p != PATTERN_START  &&  *p != ','  &&  *p != '\0')
		++p;
	return p;
}

/* Parses the mapping beginning at `map', adds it to the language map, and
 * returns first character past the map.
 */
static char* extractMapFromParameter (const langType language,
				      char* parameter,
				      char** tail,
				      boolean* pattern_p,
				      char* (* skip) (char *))
{
	char* p = NULL;
	const char first = *parameter;
	char  tmp;
	char* result;

	if (first == EXTENSION_SEPARATOR)  /* extension map */
	{
		*pattern_p = FALSE;

		++parameter;
		p = (* skip) (parameter);
		if (*p == '\0')
		{
			result = eStrdup (parameter);
			verbose (" .%s", result);
			*tail = parameter + strlen (parameter);
			return result;
		}
		else
		{
			tmp = *p;
			*p = '\0';
			result = eStrdup (parameter);
			verbose (" .%s", result);
			*p = tmp;
			*tail = p;
			return result;
		}
	}
	else if (first == PATTERN_START)  /* pattern map */
	{
		*pattern_p = TRUE;

		++parameter;
		for (p = parameter  ;  *p != PATTERN_STOP  &&  *p != '\0'  ;  ++p)
		{
			if (*p == '\\'  &&  *(p + 1) == PATTERN_STOP)
				++p;
		}
		if (*p == '\0')
			error (FATAL, "Unterminated file name pattern for %s language",
			   getLanguageName (language));
		else
		{
			tmp = *p;
			*p = '\0';
			result = eStrdup (parameter);
			verbose (" (%s)", result);
			*p = tmp;
			*tail = p + 1;
			return result;
		}
	}

	return NULL;
}

static char* addLanguageMap (const langType language, char* map_parameter)
{
	char* p = NULL;
	boolean pattern_p;
	char* map;

	map = extractMapFromParameter (language, map_parameter, &p, &pattern_p, skipPastMap);
	if (map && pattern_p == FALSE)
		addLanguageExtensionMap (language, map);
	else if (map && pattern_p == TRUE)
		addLanguagePatternMap (language, map);
	else
		error (FATAL, "Badly formed language map for %s language",
				getLanguageName (language));

	if (map)
		eFree (map);
	return p;
}

static char* processLanguageMap (char* map)
{
	char* const separator = strchr (map, ':');
	char* result = NULL;
	if (separator != NULL)
	{
		langType language;
		char *list = separator + 1;
		boolean clear = FALSE;
		*separator = '\0';
		language = getNamedLanguage (map);
		if (language != LANG_IGNORE)
		{
			const char *const deflt = "default";
			char* p;
			if (*list == '+')
				++list;
			else
				clear = TRUE;
			for (p = list  ;  *p != ','  &&  *p != '\0'  ;  ++p)  /*no-op*/ ;
			if ((size_t) (p - list) == strlen (deflt) &&
				strncasecmp (list, deflt, p - list) == 0)
			{
				verbose ("    Restoring default %s language map: ", getLanguageName (language));
				installLanguageMapDefault (language);
				list = p;
			}
			else
			{
				if (clear)
				{
					verbose ("    Setting %s language map:", getLanguageName (language));
					clearLanguageMap (language);
				}
				else
					verbose ("    Adding to %s language map:", getLanguageName (language));
				while (list != NULL  &&  *list != '\0'  &&  *list != ',')
					list = addLanguageMap (language, list);
				verbose ("\n");
			}
			if (list != NULL  &&  *list == ',')
				++list;
			result = list;
		}
	}
	return result;
}

static void processLanguageMapOption (
		const char *const option, const char *const parameter)
{
	char *const maps = eStrdup (parameter);
	char *map = maps;

	if (strcmp (parameter, "default") == 0)
	{
		verbose ("    Restoring default language maps:\n");
		installLanguageMapDefaults ();
	}
	else while (map != NULL  &&  *map != '\0')
	{
		char* const next = processLanguageMap (map);
		if (next == NULL)
			error (WARNING, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		map = next;
	}
	eFree (maps);
}

static void processLanguagesOption (
		const char *const option, const char *const parameter)
{
	char *const langs = eStrdup (parameter);
	enum { Add, Remove, Replace } mode = Replace;
	boolean first = TRUE;
	char *lang = langs;
	const char* prefix = "";
	verbose ("    Enabled languages: ");
	while (lang != NULL)
	{
		char *const end = strchr (lang, ',');
		if (lang [0] == '+')
		{
			++lang;
			mode = Add;
			prefix = "+ ";
		}
		else if (lang [0] == '-')
		{
			++lang;
			mode = Remove;
			prefix = "- ";
		}
		if (mode == Replace)
			enableLanguages (FALSE);
		if (end != NULL)
			*end = '\0';
		if (lang [0] != '\0')
		{
			if (strcmp (lang, "all") == 0)
				enableLanguages ((boolean) (mode != Remove));
			else
			{
				const langType language = getNamedLanguage (lang);
				if (language == LANG_IGNORE)
					error (WARNING, "Unknown language \"%s\" in \"%s\" option", lang, option);
				else
					enableLanguage (language, (boolean) (mode != Remove));
			}
			verbose ("%s%s%s", (first ? "" : ", "), prefix, lang);
			prefix = "";
			first = FALSE;
			if (mode == Replace)
				mode = Add;
		}
		lang = (end != NULL ? end + 1 : NULL);
	}
	verbose ("\n");
	eFree (langs);
}

extern boolean processMapOption (
			const char *const option, const char *const parameter)
{
	const char* const dash = strchr (option, '-');
	langType language;
	vString* langName;
	size_t len;
	const char* spec;
	char* map_parameter;
	boolean clear = FALSE;

	if (dash == NULL ||
	    (strcmp (dash + 1, "map") != 0))
		return FALSE;
	len = dash - option;

	langName = vStringNew ();
	vStringNCopyS (langName, option, len);
	language = getNamedLanguage (vStringValue (langName));
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
	vStringDelete(langName);

	if (parameter == NULL || parameter [0] == '\0')
		error (FATAL, "no parameter is given for %s", option);

	spec = parameter;
	if (*spec == '+')
		spec++;
	else
		clear = TRUE;

	if (clear)
	{
		verbose ("    Setting %s language map:", getLanguageName (language));
		clearLanguageMap (language);
	}
	else
		verbose ("    Adding to %s language map:", getLanguageName (language));
	map_parameter = eStrdup (spec);
	addLanguageMap (language, map_parameter);
	eFree (map_parameter);
	verbose ("\n");

	return TRUE;
}

static char* skipTillColon (char* p)
{
	while (*p != ':'  && *p != '\0')
		++p;
	return p;
}

extern boolean processCorpusOption (
		const char *const option, const char *const parameter)
{
	const char* const dash = strchr (option, '-');
	langType language;
	vString* langName;
	char* parm;
	char* colon;
	size_t len;
	const char* file_part;
	vString* tg_file;
	char* spec;
	boolean pattern_p;

	if (dash == NULL ||
	    (strcmp (dash + 1, "corpus") != 0))
		return FALSE;
	len = dash - option;

	langName = vStringNew ();
	vStringNCopyS (langName, option, len);
	language = getNamedLanguage (vStringValue (langName));
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
	vStringDelete(langName);

	if (parameter == NULL || parameter [0] == '\0')
		error (FATAL, "no parameter is given for %s", option);

	parm = eStrdup (parameter);
	spec = extractMapFromParameter (language, parm, &colon, &pattern_p, skipTillColon);
	if (spec == NULL)
		error (FATAL, "Badly formed language map specification for loading colon for %s language",
		       getLanguageName (language));
	else if (pattern_p && (*colon != ':'))
		error (FATAL,
		       "no colon(:) separator is found in parameter of %s: %s", option, parameter);
	else if ((!pattern_p) && *colon == '\0')
		error (FATAL,
		       "no colon(:) separator is found in parameter of %s: %s", option, parameter);

	file_part = colon + 1;
	if (file_part[0] == '\0')
		error (FATAL,
		       "file part after colon it not given %s: %s", option, parameter);

	if (file_part[0] != '/' && file_part[0] != '.')
	{
		tg_file = expandOnCorpusPathList (file_part);
		tg_file = tg_file? tg_file: vStringNewInit (file_part);
	}
	else
		tg_file = vStringNewInit (file_part);

	addCorpusFile (language, spec, tg_file, pattern_p);

	eFree (spec);
	eFree (parm);
	return TRUE;
}

static void processLicenseOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	puts ("");
	puts (License1);
	puts (License2);
	exit (0);
}

static void processListAliasesOption (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageAliases (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageAliases (language);
	}
	exit (0);
}

static void processListKindsOption (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
	    printLanguageKinds (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageKinds (language);
	}
	exit (0);
}

static void processListMapsOption (
		const char *const __unused__ option,
		const char *const __unused__ parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
	    printLanguageMaps (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageMaps (language);
	}
	exit (0);
}

static void processListCorporaOption (const char *const __unused__ option ,
				      const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
		printLanguageCorpus (LANG_AUTO, NULL);
	else
	{
		char* const colon = strrchr (parameter, ':');
		const char* spec  = NULL;
		langType language;

		if (colon)
		{
			*colon = '\0';
			spec = colon + 1;
		}

		language = getNamedLanguage (parameter);

		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageCorpus (language, spec);
	}
	exit (0);
}

static void processListLanguagesOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printLanguageList ();
	exit (0);
}


static void freeSearchPathList (searchPathList** pathList)
{
	stringListClear (*pathList);
	stringListDelete (*pathList);
	*pathList = NULL;
}

static void verboseSearchPathList (const searchPathList* pathList, const char *const varname)
{
	unsigned int i;

	verbose ("Install %s:\n", varname);
	for (i = 0; i < stringListCount (pathList); ++i)
		verbose ("  %s\n", vStringValue (stringListItem (pathList, i)));
}

static vString* expandOnSearchPathList (searchPathList *pathList, const char* leaf,
					boolean (* check) (const char *const))
{
	unsigned int i;

	for (i = 0; i < stringListCount (pathList); ++i)
	{
		const char* const body = vStringValue (stringListItem (pathList, i));
		char* tmp = combinePathAndFile (body, leaf);

		if ((* check) (tmp))
		{
			vString *r = vStringNewOwn (tmp);
			return r;
		}
		else
			eFree (tmp);
	}
	return NULL;
}

static boolean isDirectory (const char *const dirName)
{
	fileStatus *status = eStat (dirName);
	return status->exists && status->isDirectory;
}

static vString* expandOnOptlibPathList (const char* leaf)
{
	vString* r;
	vString* leaf_with_suffix;

	leaf_with_suffix = vStringNewInit (leaf);
	vStringCatS (leaf_with_suffix, ".d");

	r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
				    isDirectory);

	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".conf");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}

	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".ctags");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}

#ifdef MSDOS_STYLE_PATH
	if (!r)
	{
		vStringCopyS (leaf_with_suffix, leaf);
		vStringCatS (leaf_with_suffix, ".cnf");
		r = expandOnSearchPathList (OptlibPathList, vStringValue (leaf_with_suffix),
					    doesFileExist);
	}
#endif

	vStringDelete (leaf_with_suffix);

	return r;
}

extern vString* expandOnCorpusPathList (const char* leaf)
{
  return expandOnSearchPathList (CorpusPathList, leaf, doesFileExist);
}

extern vString* expandOnDriversPathList (const char* leaf)
{
	return expandOnSearchPathList (DriversPathList, leaf, doesExecutableExist);
}

static void processOptionFile (
		const char *const option, const char *const parameter)
{
	const char* path;
	vString* vpath = NULL;
	fileStatus *status;

	if (parameter [0] == '\0')
		error (FATAL, "no option file supplied for \"%s\"", option);

	if (parameter [0] != '/' && parameter [0] != '.')
	{
		vpath = expandOnOptlibPathList (parameter);
		path = vpath? vStringValue (vpath): parameter;
	}
	else
		path = parameter;

	status = eStat (path);
	if (!status->exists)
	{
		error (FATAL | PERROR, "cannot stat \"%s\"", path);
	}
	else if (status->isDirectory)
	{
		if (!parseAllConfigurationFilesOptionsInDirectory (path, NULL))
			error (FATAL | PERROR, "cannot open option directory \"%s\"", path);
	}
	else
	{
		if (!parseFileOptions (path))
			error (FATAL | PERROR, "cannot open option file \"%s\"", path);
	}

	eStatFree (status);
	if (vpath)
		vStringDelete (vpath);
}

static void processSortOption (
		const char *const option, const char *const parameter)
{
	if (isFalse (parameter))
		Option.sorted = SO_UNSORTED;
	else if (isTrue (parameter))
		Option.sorted = SO_SORTED;
	else if (strcasecmp (parameter, "f") == 0 ||
			strcasecmp (parameter, "fold") == 0 ||
			strcasecmp (parameter, "foldcase") == 0)
		Option.sorted = SO_FOLDSORTED;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);
}

static void installHeaderListDefaults (void)
{
	Option.headerExt = stringListNewFromArgv (HeaderExtensions);
	if (Option.verbose)
	{
		fprintf (stderr, "    Setting default header extensions: ");
		stringListPrint (Option.headerExt, stderr);
		putc ('\n', stderr);
	}
}

static void processHeaderListOption (const int option, const char *parameter)
{
	/*  Check to make sure that the user did not enter "ctags -h *.c"
	 *  by testing to see if the list is a filename that exists.
	 */
	if (doesFileExist (parameter))
		error (FATAL, "-%c: Invalid list", option);
	if (strcmp (parameter, "default") == 0)
		installHeaderListDefaults ();
	else
	{
		boolean clear = TRUE;

		if (parameter [0] == '+')
		{
			++parameter;
			clear = FALSE;
		}
		if (Option.headerExt == NULL)
			Option.headerExt = stringListNew ();
		verbose ("    Header Extensions:\n");
		addExtensionList (Option.headerExt, parameter, clear);
	}
}

/*
 *  Token ignore processing
 */

/*  Determines whether or not "name" should be ignored, per the ignore list.
 */
extern boolean isIgnoreToken (
		const char *const name, boolean *const pIgnoreParens,
		const char **const replacement)
{
	boolean result = FALSE;

	if (Option.ignore != NULL)
	{
		const size_t nameLen = strlen (name);
		unsigned int i;

		if (pIgnoreParens != NULL)
			*pIgnoreParens = FALSE;

		for (i = 0  ;  i < stringListCount (Option.ignore)  ;  ++i)
		{
			vString *token = stringListItem (Option.ignore, i);

			if (strncmp (vStringValue (token), name, nameLen) == 0)
			{
				const size_t tokenLen = vStringLength (token);

				if (nameLen == tokenLen)
				{
					result = TRUE;
					break;
				}
				else if (tokenLen == nameLen + 1  &&
						vStringChar (token, tokenLen - 1) == '+')
				{
					result = TRUE;
					if (pIgnoreParens != NULL)
						*pIgnoreParens = TRUE;
					break;
				}
				else if (vStringChar (token, nameLen) == '=')
				{
					if (replacement != NULL)
						*replacement = vStringValue (token) + nameLen + 1;
					break;
				}
			}
		}
	}
	return result;
}

static void saveIgnoreToken (vString *const ignoreToken)
{
	if (Option.ignore == NULL)
		Option.ignore = stringListNew ();
	stringListAdd (Option.ignore, ignoreToken);
	verbose ("    ignore token: %s\n", vStringValue (ignoreToken));
}

static void readIgnoreList (const char *const list)
{
	char* newList = stringCopy (list);
	const char *token = strtok (newList, IGNORE_SEPARATORS);

	while (token != NULL)
	{
		vString *const entry = vStringNewInit (token);

		saveIgnoreToken (entry);
		token = strtok (NULL, IGNORE_SEPARATORS);
	}
	eFree (newList);
}

static void addIgnoreListFromFile (const char *const fileName)
{
	stringList* tokens = stringListNewFromFile (fileName);
	if (tokens == NULL)
		error (FATAL | PERROR, "cannot open \"%s\"", fileName);
	if (Option.ignore == NULL)
		Option.ignore = tokens;
	else
		stringListCombine (Option.ignore, tokens);
}

static void processIgnoreOption (const char *const list)
{
	if (strchr ("@./\\", list [0]) != NULL)
	{
		const char* fileName = (*list == '@') ? list + 1 : list;
		addIgnoreListFromFile (fileName);
	}
#if defined (MSDOS) || defined (WIN32) || defined (OS2)
	else if (isalpha (list [0])  &&  list [1] == ':')
		addIgnoreListFromFile (list);
#endif
	else if (strcmp (list, "-") == 0)
	{
		freeList (&Option.ignore);
		verbose ("    clearing list\n");
	}
	else
		readIgnoreList (list);
}

static void processVersionOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	exit (0);
}

static void resetPathList (searchPathList** pathList, const char *const varname)
{
	freeSearchPathList (pathList);
	verbose ("Reset %s\n", varname);
	*pathList = stringListNew ();
}

static void resetDataPathList (void)
{
	resetPathList (&OptlibPathList, "OptlibPathList");
	resetPathList (&CorpusPathList, "CorpusPathList");
}

static void resetLibexecPathList (void)
{
	resetPathList (&DriversPathList, "DriversPathList");
}

static void appendToPathList (const char *const dir, const char *const subdir, searchPathList* const pathList, const char *const varname,
				   boolean report_in_verboe, const char* const action)
{
	char* path;

	path = combinePathAndFile (dir, subdir);
	if (report_in_verboe)
		verbose ("%s %s to %s\n", action, path, varname);
	stringListAdd (pathList, vStringNewOwn (path));
}

static void prependToPathList (const char *const dir, const char *const subdir, searchPathList* const pathList, const char *const varname,
				    boolean report_in_verboe, const char* const action)
{
	stringListReverse (pathList);
	appendToPathList(dir, subdir, pathList, varname, report_in_verboe, action);
	stringListReverse (pathList);

}

static void appendToDataPathList (const char *const dir, boolean from_cmdline)
{
	appendToPathList (dir, SUBDIR_OPTLIB, OptlibPathList, "OptlibPathList",
			       from_cmdline, from_cmdline? "Append": NULL);
	appendToPathList (dir, SUBDIR_CORPORA, CorpusPathList, "CorpusPathList",
			       from_cmdline, from_cmdline? "Append": NULL);

	if (!from_cmdline)
		appendToPathList (dir, SUBDIR_PRELOAD, PreloadPathList, "PreloadPathList",
				       FALSE, NULL);
}

static void appendToLibexecPathList (const char *const dir, boolean from_cmdline)
{
	appendToPathList (dir, SUBDIR_DRIVERS, DriversPathList, "DriversPathList",
			       from_cmdline, from_cmdline? "Append": NULL);
}

static void prependToDataPathList (const char *const dir, boolean from_cmdline)
{
	prependToPathList (dir, SUBDIR_OPTLIB, OptlibPathList, "OptlibPathList",
				from_cmdline, from_cmdline? "Prepend": NULL);
	prependToPathList (dir, SUBDIR_CORPORA, CorpusPathList, "CorpusPathList",
				from_cmdline, from_cmdline? "Prepend": NULL);

	if (!from_cmdline)
		prependToPathList (dir, SUBDIR_PRELOAD, PreloadPathList, "PreloadPathList",
					FALSE, NULL);
}

static void prependToLibexecPathList (const char *const dir, boolean from_cmdline)
{
	prependToPathList (dir, SUBDIR_DRIVERS, DriversPathList, "DriversPathList",
			   from_cmdline, from_cmdline? "Prepend": NULL);
}

static void processDataDir (
		const char *const option, const char *const parameter)
{
	const char* path;

	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "Path for a directory is needed for \"%s\" option", option);

	if (parameter[0] == '+')
	{
		path = parameter + 1;
		prependToDataPathList (path, TRUE);
	}
	else if (!strcmp (parameter, "NONE"))
		resetDataPathList ();
	else
	{
		resetDataPathList ();
		path = parameter;
		appendToDataPathList (path, TRUE);
	}
}

static void processLibexecDir (const char *const option,
			       const char *const parameter)
{
	const char* path;

	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "Path for a directory is needed for \"%s\" option", option);

	if (parameter[0] == '+')
	{
		path = parameter + 1;
		prependToLibexecPathList (path, TRUE);
	}
	else if (!strcmp (parameter, "NONE"))
		resetLibexecPathList ();
	else
	{
		resetLibexecPathList ();
		path = parameter;
		appendToLibexecPathList (path, TRUE);
	}
}

/*
 *  Option tables
 */

static parametricOption ParametricOptions [] = {
	{ "config-filename",      	processConfigFilenameOption,  	TRUE    },
	{ "data-dir",               processDataDir,                 FALSE   },
	{ "etags-include",          processEtagsInclude,            FALSE   },
	{ "exclude",                processExcludeOption,           FALSE   },
	{ "excmd",                  processExcmdOption,             FALSE   },
	{ "extra",                  processExtraTagsOption,         FALSE   },
	{ "fields",                 processFieldsOption,            FALSE   },
	{ "filter-terminator",      processFilterTerminatorOption,  TRUE    },
	{ "format",                 processFormatOption,            TRUE    },
	{ "help",                   processHelpOption,              TRUE    },
	{ "lang",                   processLanguageForceOption,     FALSE   },
	{ "language",               processLanguageForceOption,     FALSE   },
	{ "language-force",         processLanguageForceOption,     FALSE   },
	{ "languages",              processLanguagesOption,         FALSE   },
	{ "langdef",                processLanguageDefineOption,    FALSE   },
	{ "langmap",                processLanguageMapOption,       FALSE   },
	{ "libexec-dir",            processLibexecDir,              FALSE   },
	{ "license",                processLicenseOption,           TRUE    },
	{ "list-aliases",           processListAliasesOption,       TRUE    },
	{ "list-corpora",           processListCorporaOption,       TRUE    },
	{ "list-features",          processListFeaturesOption,      TRUE    },
	{ "list-kinds",             processListKindsOption,         TRUE    },
	{ "list-languages",         processListLanguagesOption,     TRUE    },
	{ "list-maps",              processListMapsOption,          TRUE    },
	{ "options",                processOptionFile,              FALSE   },
	{ "sort",                   processSortOption,              TRUE    },
	{ "version",                processVersionOption,           TRUE    },
};

static booleanOption BooleanOptions [] = {
	{ "append",         &Option.append,                 TRUE    },
	{ "file-scope",     &Option.include.fileScope,      FALSE   },
	{ "file-tags",      &Option.include.fileNames,      FALSE   },
	{ "filter",         &Option.filter,                 TRUE    },
	{ "guess-language-eagerly", &Option.guessLanguageEagerly, TRUE },
	{ "if0",            &Option.if0,                    FALSE   },
	{ "kind-long",      &Option.kindLong,               TRUE    },
	{ "line-directives",&Option.lineDirectives,         FALSE   },
	{ "links",          &Option.followLinks,            FALSE   },
	{ "print-language", &Option.printLanguage,          TRUE    },
#ifdef RECURSE_SUPPORTED
	{ "recurse",        &Option.recurse,                FALSE   },
#endif
	{ "tag-relative",   &Option.tagRelative,            TRUE    },
	{ "totals",         &Option.printTotals,            TRUE    },
	{ "verbose",        &Option.verbose,                FALSE   },
};

/*
 *  Generic option parsing
 */

static void checkOptionOrder (const char* const option)
{
	if (NonOptionEncountered)
		error (FATAL, "-%s option may not follow a file name", option);
}

static boolean processParametricOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (ParametricOptions) / sizeof (parametricOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		parametricOption* const entry = &ParametricOptions [i];
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (entry->initOnly)
				checkOptionOrder (option);
			(entry->handler) (option, parameter);
		}
	}
	return found;
}

static boolean getBooleanOption (
		const char *const option, const char *const parameter)
{
	boolean selection = TRUE;

	if (parameter [0] == '\0')
		selection = TRUE;
	else if (isFalse (parameter))
		selection = FALSE;
	else if (isTrue (parameter))
		selection = TRUE;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);

	return selection;
}

static boolean processBooleanOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (BooleanOptions) / sizeof (booleanOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		booleanOption* const entry = &BooleanOptions [i];
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (entry->initOnly)
				checkOptionOrder (option);
			*entry->pValue = getBooleanOption (option, parameter);
		}
	}
	return found;
}

static void processLongOption (
		const char *const option, const char *const parameter)
{
	Assert (parameter != NULL);
	if (parameter == NULL  &&  parameter [0] == '\0')
		verbose ("  Option: --%s\n", option);
	else
		verbose ("  Option: --%s=%s\n", option, parameter);

	if (processBooleanOption (option, parameter))
		;
	else if (processParametricOption (option, parameter))
		;
	else if (processKindOption (option, parameter))
		;
	else if (processCorpusOption (option, parameter))
		;
	else if (processAliasOption (option, parameter))
		;
	else if (processRegexOption (option, parameter))
		;
	else if (processXcmdOption (option, parameter))
		;
	else if (processMapOption (option, parameter))
		;
#ifndef RECURSE_SUPPORTED
	else if (strcmp (option, "recurse") == 0)
		error (WARNING, "%s option not supported on this host", option);
#endif
	else
		error (FATAL, "Unknown option: --%s", option);
}

static void processShortOption (
		const char *const option, const char *const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		verbose ("  Option: -%s\n", option);
	else
		verbose ("  Option: -%s %s\n", option, parameter);

	if (isCompoundOption (*option) && (parameter == NULL  ||  parameter [0] == '\0'))
		error (FATAL, "Missing parameter for \"%s\" option", option);
	else switch (*option)
	{
		case '?':
			processHelpOption ("?", NULL);
			exit (0);
			break;
		case 'a':
			checkOptionOrder (option);
			Option.append = TRUE;
			break;
#ifdef DEBUG
		case 'b':
			if (atol (parameter) < 0)
				error (FATAL, "-%s: Invalid line number", option);
			Option.breakLine = atol (parameter);
			break;
		case 'D':
			Option.debugLevel = strtol (parameter, NULL, 0);
			if (debug (DEBUG_STATUS))
				Option.verbose = TRUE;
			break;
#endif
		case 'B':
			Option.backward = TRUE;
			break;
		case 'e':
			checkOptionOrder (option);
			setEtagsMode ();
			break;
		case 'f':
		case 'o':
			checkOptionOrder (option);
			if (Option.tagFileName != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.tagFileName);
			}
			else if (parameter [0] == '-'  &&  parameter [1] != '\0')
				error (FATAL, "output file name may not begin with a '-'");
			Option.tagFileName = stringCopy (parameter);
			break;
		case 'F':
			Option.backward = FALSE;
			break;
		case 'G':
			Option.guessLanguageEagerly = TRUE;
			break;
		case 'h':
			processHeaderListOption (*option, parameter);
			break;
		case 'I':
			processIgnoreOption (parameter);
			break;
		case 'L':
			if (Option.fileList != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.fileList);
			}
			Option.fileList = stringCopy (parameter);
			break;
		case 'n':
			Option.locate = EX_LINENUM;
			break;
		case 'N':
			Option.locate = EX_PATTERN;
			break;
		case 'R':
#ifdef RECURSE_SUPPORTED
			Option.recurse = TRUE;
#else
			error (WARNING, "-%s option not supported on this host", option);
#endif
			break;
		case 'u':
			checkOptionOrder (option);
			Option.sorted = SO_UNSORTED;
			break;
		case 'V':
			Option.verbose = TRUE;
			break;
		case 'w':
			/* silently ignored */
			break;
		case 'x':
			checkOptionOrder (option);
			Option.xref = TRUE;
			break;
		default:
			error (FATAL, "Unknown option: -%s", option);
			break;
	}
}

extern void parseOption (cookedArgs* const args)
{
	Assert (! cArgOff (args));
	if (args->isOption)
	{
		if (args->longOption)
			processLongOption (args->item, args->parameter);
		else
		{
			const char *parameter = args->parameter;
			while (*parameter == ' ')
				++parameter;
			processShortOption (args->item, parameter);
		}
		cArgForth (args);
	}
}

extern void parseOptions (cookedArgs* const args)
{
	NonOptionEncountered = FALSE;
	while (! cArgOff (args)  &&  cArgIsOption (args))
		parseOption (args);
	if (! cArgOff (args)  &&  ! cArgIsOption (args))
		NonOptionEncountered = TRUE;
}

static const char *CheckFile;
static boolean checkSameFile (const char *const fileName)
{
	return isSameFile (CheckFile, fileName);
}

static boolean parseFileOptions (const char* const fileName)
{
	boolean fileFound = FALSE;
	const char* const format = "Considering option file %s: %s\n";
	CheckFile = fileName;
	if (stringListHasTest (OptionFiles, checkSameFile))
		verbose (format, fileName, "already considered");
	else
	{
		FILE* const fp = fopen (fileName, "r");
		if (fp == NULL)
			verbose (format, fileName, "not found");
		else
		{
			cookedArgs* const args = cArgNewFromLineFile (fp);
			vString* file = vStringNewInit (fileName);
			stringListAdd (OptionFiles, file);
			verbose (format, fileName, "reading...");
			parseOptions (args);
			if (NonOptionEncountered)
				error (WARNING, "Ignoring non-option in %s\n", fileName);
			cArgDelete (args);
			fclose (fp);
			fileFound = TRUE;
		}
	}
	return fileFound;
}

/* Actions to be taken before reading any other options */
extern void previewFirstOption (cookedArgs* const args)
{
	while (cArgIsOption (args))
	{
		if (strcmp (args->item, "V") == 0 || strcmp (args->item, "verbose") == 0 || strcmp (args->item, "config-filename") == 0 )
			parseOption (args);
		else if (strcmp (args->item, "options") == 0  &&
				strcmp (args->parameter, "NONE") == 0)
		{
			fprintf (stderr, "No options will be read from files or environment\n");
			SkipConfiguration = TRUE;
			cArgForth (args);
		}
		else
			break;
	}
}

static void parseConfigurationFileOptionsInDirectoryWithLeafname (const char* directory, const char* leafname)
{
	char* pathname = combinePathAndFile (directory, leafname);
	parseFileOptions (pathname);
	eFree (pathname);
}

static void parseConfigurationFileOptionsInDirectory (const char* directory)
{
	char	*leafname = NULL;

	if (asprintf (&leafname,".%s",(Option.configFilename)?Option.configFilename:"ctags") == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#ifdef MSDOS_STYLE_PATH
	if (asprintf (&leafname,"%s.cnf",(Option.configFilename)?Option.configFilename:"ctags") == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#endif
}

#if defined(HAVE_SCANDIR)
static int ignore_dot_file(const struct dirent* dent)
{
	/* Ignore a file which name is started from dot. */
	if (*dent->d_name == '.')
		return 0;
	else
		return 1;
}

static int accept_only_dot_d(const struct dirent* dent)
{
	size_t len;

	/* accept only a directory ended with ".d" */
	len = strlen(dent->d_name);

	if (len < 3)
		return 0;
	return !strcmp(dent->d_name + (len - 2), ".d");
}

static int accept_only_dot_ctags(const struct dirent* dent)
{
	size_t len;

	/* accept only a file ended with ".conf" or ".ctags" */
	len = strlen(dent->d_name);

	if (len < 6)
		return 0;
	if (strcmp(dent->d_name + (len - 5), ".conf") == 0)
		return 1;

	if (len < 7)
		return 0;
	if (strcmp(dent->d_name + (len - 6), ".ctags") == 0)
		return 1;

	return 0;
}

static boolean parseAllConfigurationFilesOptionsInDirectory (const char* const dirName,
							     stringList* const already_loaded_files)
{
	struct dirent **dents;
	int i, n;

	n = scandir (dirName, &dents, ignore_dot_file, alphasort);
	if (n < 0)
		return FALSE;
	
	for (i = 0; i < n; i++)
	{
		char* path;
		fileStatus *s;

		if (already_loaded_files && stringListHas (already_loaded_files, dents[i]->d_name))
			continue;
		else if (already_loaded_files)
			stringListAdd (already_loaded_files, vStringNewInit (dents[i]->d_name));

		path = combinePathAndFile (dirName, dents[i]->d_name);
		s = eStat (path);

		if (s->exists && s->isDirectory && accept_only_dot_d(dents[i]))
			parseAllConfigurationFilesOptionsInDirectory (path,
								      already_loaded_files);
		else if (s->exists && accept_only_dot_ctags(dents[i]))
			parseConfigurationFileOptionsInDirectoryWithLeafname (dirName,
									      dents[i]->d_name);
		eStatFree (s);
		free (dents[i]);
		eFree (path);
	}
	free (dents);
	return TRUE;
}
#else
static boolean parseAllConfigurationFilesOptionsInDirectory (const char* const dirName,
							     stringList* const already_loaded_files)
{
	return FALSE;
}
#endif

static void preload (const searchPathList *const pathList)
{
	unsigned int i;
	stringList* loaded;

	loaded = stringListNew ();
	for (i = 0; i < stringListCount (pathList); ++i)
	{
		const char* const dir = vStringValue (stringListItem (pathList, i));
		parseAllConfigurationFilesOptionsInDirectory (dir, loaded);
	}
	stringListClear (loaded);
	stringListDelete (loaded);
}

static void parseConfigurationFileOptions (void)
{
	vString *home;
	/* We parse .ctags on all systems, and additionally ctags.cnf on DOS. */
	char *filename = NULL;
	const char *filename_body;

#ifdef CUSTOM_CONFIGURATION_FILE
	parseFileOptions (CUSTOM_CONFIGURATION_FILE);
#endif
	filename_body = (Option.configFilename)?Option.configFilename:"ctags";
#ifdef MSDOS_STYLE_PATH

	if (asprintf (&filename,"/%s.cnf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseFileOptions (filename);
	free (filename);
#endif
	if (asprintf (&filename,"/etc/%s.conf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseFileOptions (filename);
	free (filename);

	if (asprintf (&filename,"/usr/local/etc/%s.conf", filename_body) == -1)
	{
		error (FATAL, "error in asprintf");
	}
	parseFileOptions (filename);
	free (filename);

	home = getHome ();
	if (home != NULL)
	{
		parseConfigurationFileOptionsInDirectory (vStringValue (home));
		vStringDelete (home);
	}

	parseConfigurationFileOptionsInDirectory (".");

	preload (PreloadPathList);
}

static void parseEnvironmentOptions (void)
{
	const char *envOptions = NULL;
	const char* var = NULL;

	if (Option.etags)
	{
		var = ETAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions == NULL)
	{
		var = CTAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions != NULL  &&  envOptions [0] != '\0')
	{
		cookedArgs* const args = cArgNewFromString (envOptions);
		verbose ("Reading options from $CTAGS\n");
		parseOptions (args);
		cArgDelete (args);
		if (NonOptionEncountered)
			error (WARNING, "Ignoring non-option in %s variable", var);
	}
}

extern void readOptionConfiguration (void)
{
	if (! SkipConfiguration)
	{
		parseConfigurationFileOptions ();
		parseEnvironmentOptions ();
	}
}

static void installDataPathList (void)
{
	char* dataPath = getenv (CTAGS_DATA_PATH_ENVIRONMENT);

	OptlibPathList = stringListNew ();
	CorpusPathList = stringListNew ();
	PreloadPathList = stringListNew ();

	if (dataPath)
	{
		char* needle;

		while (dataPath[0])
		{
			needle = strchr (dataPath, ':');
			if (needle)
				*needle = '\0';

			appendToDataPathList (dataPath, FALSE);

			if (needle)
			{
				*needle = ':';
				dataPath = needle + 1;
			}
			else
				break;
		}
	}

	{
		vString *home = getHome ();
		if (home != NULL)
		{
			char *ctags_d;

			ctags_d = combinePathAndFile (vStringValue (home), ".ctags.d");

			appendToDataPathList (ctags_d, FALSE);
			eFree (ctags_d);
			vStringDelete (home);
		}
	}
#ifdef PKGCONFDIR
	appendToDataPathList (PKGCONFDIR, FALSE);
#endif

#ifdef DATADIR
	appendToDataPathList (DATADIR, FALSE);
#endif
}

static void installLibexecPathList (void)
{
	char* libexecPath = getenv (CTAGS_LIBEXEC_PATH_ENVIRONMENT);

	DriversPathList = stringListNew ();

	if (libexecPath)
	{
		char* needle;

		while (libexecPath[0])
		{
			needle = strchr (libexecPath, ':');
			if (needle)
				*needle = '\0';

			appendToDataPathList (libexecPath, FALSE);

			if (needle)
			{
				*needle = ':';
				libexecPath = needle + 1;
			}
			else
				break;
		}
	}

	{
		vString *home = getHome ();
		if (home != NULL)
		{
			char *ctags_d;

			ctags_d = combinePathAndFile (vStringValue (home), ".ctags.d");

			appendToLibexecPathList (ctags_d, FALSE);
			eFree (ctags_d);
			vStringDelete (home);
		}
	}

#ifdef PKGLIBEXECDIR
	appendToLibexecPathList (PKGLIBEXECDIR, FALSE);
#endif
}

/*
*   Option initialization
*/

extern void initOptions (void)
{
	OptionFiles = stringListNew ();
	installDataPathList ();
	installLibexecPathList ();
	verboseSearchPathList (OptlibPathList,  "OptlibPathList");
	verboseSearchPathList (CorpusPathList,  "CorpusPathList");
	verboseSearchPathList (PreloadPathList, "PreloadPathList");
	verboseSearchPathList (DriversPathList, "DriversPathList");

	verbose ("Setting option defaults\n");
	installHeaderListDefaults ();
	verbose ("  Installing default language mappings:\n");
	installLanguageMapDefaults ();
	verbose ("  Installing default language aliases:\n");
	installLanguageAliasesDefaults ();

	/* always excluded by default */
	verbose ("  Installing default exclude patterns:\n");
	processExcludeOption (NULL, "{arch}");
	processExcludeOption (NULL, ".arch-ids");
	processExcludeOption (NULL, ".arch-inventory");
	processExcludeOption (NULL, "autom4te.cache");
	processExcludeOption (NULL, "BitKeeper");
	processExcludeOption (NULL, ".bzr");
	processExcludeOption (NULL, ".bzrignore");
	processExcludeOption (NULL, "CVS");
	processExcludeOption (NULL, ".cvsignore");
	processExcludeOption (NULL, "_darcs");
	processExcludeOption (NULL, ".deps");
	processExcludeOption (NULL, "EIFGEN");
	processExcludeOption (NULL, ".git");
	processExcludeOption (NULL, ".hg");
	processExcludeOption (NULL, "PENDING");
	processExcludeOption (NULL, "RCS");
	processExcludeOption (NULL, "RESYNC");
	processExcludeOption (NULL, "SCCS");
	processExcludeOption (NULL, ".svn");
}

extern void freeOptionResources (void)
{
	freeString (&Option.tagFileName);
	freeString (&Option.fileList);
	freeString (&Option.filterTerminator);

	freeList (&Excluded);
	freeList (&Option.ignore);
	freeList (&Option.headerExt);
	freeList (&Option.etagsInclude);

	freeSearchPathList (&CorpusPathList);
	freeSearchPathList (&OptlibPathList);
	freeSearchPathList (&PreloadPathList);
	freeSearchPathList (&DriversPathList);

	freeList (&OptionFiles);
}

/* vi:set tabstop=4 shiftwidth=4: */
