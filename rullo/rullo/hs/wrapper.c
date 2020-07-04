#include <stdlib.h>
#include "HsFFI.h"

void haskell_init(void)
{
  hs_init(NULL, NULL);
}

void haskell_end(void)
{
  hs_exit();
}