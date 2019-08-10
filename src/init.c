#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "emacs-module.h"
#include "interface.h"
#include "libefriso.h"

/* Declare mandatory GPL symbol.  */
int plugin_is_GPL_compatible;


int emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  printf("env->size:\n");
  int emacs_version;
  if (ert->size < sizeof (*ert))
    return 1;

  emacs_env *env = ert->get_environment (ert);
  if (env->size >= sizeof (struct emacs_env_26))
    {
      printf("env->size:%td \n", env->size);
      emacs_version = 26;  /* Emacs 26 or later.  */
    }
  else if (env->size >= sizeof (struct emacs_env_25))
    emacs_version = 25;
  else
    return 2; /* Unknown or unsupported version.  */

  em_init(env);

  libefriso_init(env);

  em_provide (env, "libefriso");

  /* loaded successfully */
  return 0;
}
