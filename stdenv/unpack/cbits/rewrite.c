#include "../include/store_path.h"
#include <stdint.h>
#include <string.h>

void hs_hash_rewrite(uint8_t *restrict buf, const size_t len, const uint8_t *pattern,
             const uint8_t *replace) {
  for (uint8_t *restrict i = buf; i && i + STORE_PATH_HASH_LEN < buf + len;
       i = memchr(i + 1, pattern[0], len - (i + 1 - buf))) {
    for (int j = 1; j < STORE_PATH_HASH_LEN; ++j) {
      if (i[j] != pattern[j])
        goto fail;
    }
    memcpy(i, replace, STORE_PATH_HASH_LEN);
    fail:;
  }
}
