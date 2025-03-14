/* #include "friso_API.h" */
#include "emacs-module.h"
#include "friso.h"
#include "interface.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define DEFUN(lsym, csym, amin, amax, doc, data)                               \
  bind_function(env, lsym, env->make_function(env, amin, amax, csym, doc, data))

typedef struct _EmacsFriso {
  friso_t friso;
  friso_config_t config;
  friso_task_t task;
  bool firstRun;
} EmacsFriso;

/* Copy a string out of an emacs argument. Returns a new pointer to a string on
 * the heap. It's the user's responsibility to free this pointer. */
static const char *copy_string_from_emacs(emacs_env *env, emacs_value arg) {
  ptrdiff_t s = 1024;
  char *str = (char *)malloc(s), *new_str;
  if (str == NULL) {
    emacs_value signal = env->intern(env, "error");
    const char *s = "malloc() failed!";
    emacs_value msg = env->make_string(env, s, strlen(s));
    env->non_local_exit_signal(env, signal, msg);
    return NULL;
  }

  for (int i = 0; i < 2; i++) {
    if (env->copy_string_contents(env, arg, str, &s)) {
      return str;
    }

    /* The buffer wasn't big enough to capture the string from
     * emacs. Realloc and try again. */
    new_str = (char *)realloc(str, s);
    if (new_str == NULL) {
      free(str);
      emacs_value signal = env->intern(env, "error");
      const char *s = "realloc() failed!";
      emacs_value msg = env->make_string(env, s, strlen(s));
      env->non_local_exit_signal(env, signal, msg);
      return NULL;
    }
    str = new_str; /* TODO: use reallocf */
  }

  return NULL;
}

static emacs_value Fload_dict(emacs_env *env, ptrdiff_t nargs,
                              emacs_value args[], void *data) {
  EmacsFriso *handle = (EmacsFriso *)data;
  printf("fail to initialize friso and config.\n");
  friso_t friso = friso_new();
  friso_config_t config = friso_new_config();
  const char* path = copy_string_from_emacs(env, args[0]);
  if (friso_init_from_ifile(friso, config, (fstring)path) != 1) {
    printf("fail to initialize friso and config.\n");
    return em_nil;
  }
  handle->friso = friso;
  handle->config = config;
  return em_t;
}

static emacs_value
make_lisp_vector(emacs_env *env, emacs_value args[], size_t size)
{
  emacs_value Qvector = env->intern(env, "vector");
  return env->funcall(env, Qvector, size, args);
}

static emacs_value Fsplit_words(emacs_env *env, ptrdiff_t nargs,
                                emacs_value args[], void *data) {
  EmacsFriso *handle = (EmacsFriso*) data;
  /* check is ARG1 a ptr of Jieba */
  /* if (!is_jieba(env, args[0], data)) { */
  /*   emacs_value signal = env->intern(env, "wrong-type-argument"); */
  /*   env->non_local_exit_signal(env, signal, args[0]); */
  /*   return NULL; */
  /* } */

  /* /\* check is ARG2 of type string *\/ */
  /* if (env->non_local_exit_check(env) != emacs_funcall_exit_return) { */
  /*   emacs_value signal = env->intern(env, "wrong-type-argument"); */
  /*   env->non_local_exit_signal(env, signal, args[1]); */
  /*   return NULL; */
  /* } */
  const char *s = copy_string_from_emacs(env, args[1]);
  size_t len = strlen(s);

  friso_task_t task = friso_new_task();
  friso_set_text(task, (fstring)s);
  int i = 0;
  emacs_value list[len];
  while ((handle->config->next_token(handle->friso, handle->config, task)) !=
         NULL) {
    // printf("%s[%d, %d, %d] ", task->token->word,
    //        task->token->offset, task->token->length, task->token->rlen );
    // printf("%s ", task->token->word );
    list[i] = env->make_string(env, task->token->word, task->token->length);
    i++;
  }
  friso_free_task(task);
  return make_lisp_vector(env, list, i);
}

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
  /* Set the function cell of the symbol named NAME to SFUN using
     the 'defalias' function.  */

  /* Convert the strings to symbols by interning them */
  emacs_value Qdefalias = env->intern(env, "defalias");
  emacs_value Qsym = env->intern(env, name);

  /* Prepare the arguments array */
  emacs_value args[] = {Qsym, Sfun};

  /* Make the call (2 == nb of arguments) */
  env->funcall(env, Qdefalias, 2, args);
}

void provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern(env, feature);
  emacs_value Qprovide = env->intern(env, "provide");
  emacs_value args[] = {Qfeat};

  env->funcall(env, Qprovide, 1, args);
}

void libefriso_init(emacs_env *env) {
  EmacsFriso *friso = (EmacsFriso *)malloc(sizeof(EmacsFriso));

  DEFUN("sinoword--load-dict", Fload_dict, 1, 1,
        "Load dictionaries, then produce a friso instance.", friso);
  DEFUN("sinoword--split-words", Fsplit_words, 2, 2, "Split Words.", friso);
}
