#include <readline/history.h>
#include <readline/readline.h>
#include <stdlib.h>
#include "foreign.h"

struct entry
{
    char         *str;
    struct entry *next;
};

struct entry *dict    = NULL;
struct entry *results = NULL;

static char *
completion_generator (const char *text, int state) 
{
    char *str = NULL;
    static struct entry *current;

    if (0 == state)
    {
        current = results;

        while (NULL != current)
        {
            struct entry *next = current->next;
            free (current->str);
            free (current);
            current = next;
        }
        results = NULL;

        current = dict;

        while (NULL != current)
        {
            if (current->str == strstr (current->str, text))
            {
                struct entry *new_entry = malloc (sizeof (struct entry));
                new_entry->next = results; 
                new_entry->str = strdup (current->str);
                results = new_entry;
            }
            current = current->next;
        }
        current = results;
    }

    if (NULL != current)
    {
        str = strdup (current->str);
        current = current->next;
    }

    return str;
}

static char **
completer (const char *text, int start, int end) 
{
    rl_attempted_completion_over = 1;

    return rl_completion_matches (text, completion_generator);
}

void
readline_init ()
{
    rl_attempted_completion_function = completer;
}

void 
add_dict_entry (char *str)
{
    struct entry *entry = malloc (sizeof (struct entry));
    entry->str = strdup (str);
    entry->next = dict;
    dict = entry;
}
