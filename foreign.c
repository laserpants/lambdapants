#include "foreign.h"
#include <readline/readline.h>
#include <readline/history.h>

char *(*callback) (const char *, int);

static char *
completion_generator (const char *text, int state) 
{
    char *str;
    static int count = 0;

    if (0 == state)
        count = 0;
    if (NULL == (str = callback (text, count++)))
        return str;
    return strdup (str);
}

static char **
completer (const char *text, int start, int end) 
{
    rl_attempted_completion_over = 1;

    return rl_completion_matches (text, completion_generator);
}

void
xxx (char *(*funptr) (const char *, int))
{
    callback = funptr;

    rl_attempted_completion_function = completer;
}

char *
null ()
{
    return NULL;
}
