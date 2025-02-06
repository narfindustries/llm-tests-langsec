#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <zlib.h>

typedef uint32_t hammer_uint32;

hammer_parser uint32_parser(void) {
  return hammer_map(hammer_take(4), (hammer_map_func)hammer_uint32_from_bytes);
}

hammer_parser uint16_parser(void) {
  return hammer_map(hammer_take(2), (hammer_map_func)hammer_uint16_from_bytes);
}

hammer_parser string_parser(void) {
  return hammer_many(hammer_satisfy((hammer_satisfy_func)(char c){ return c != '\0'; }));
}

hammer_parser gzip_header_parser(void) {
  return hammer_seq(
    hammer_expect("ID1", hammer_equal(0x1f)),
    hammer_expect("ID2", hammer_equal(0x8b)),
    hammer_expect("CM", hammer_equal(0x08)),
    hammer_take(1),
    uint32_parser(),
    hammer_take(1),
    hammer_take(1),
    hammer_end()
  );
}

hammer_parser optional_fields_parser(void) {
  return hammer_choice(
    hammer_seq(
      uint16_parser(),
      hammer_take(2),
      hammer_end()
    ),
    hammer_empty()
  );
}


hammer_parser gzip_footer_parser(void) {
  return hammer_seq(
    uint32_parser(),
    uint32_parser(),
    hammer_end()
  );
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
    return 1;
  }

  FILE *fp = fopen(argv[1], "rb");
  if (fp == NULL) {
    perror("Error opening file");
    return 1;
  }

  fseek(fp, 0, SEEK_END);
  long fileSize = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  unsigned char *buffer = (unsigned char *)malloc(fileSize);
  if (buffer == NULL) {
    perror("Memory allocation failed");
    fclose(fp);
    return 1;
  }

  if (fread(buffer, 1, fileSize, fp) != fileSize) {
    perror("Error reading file");
    fclose(fp);
    free(buffer);
    return 1;
  }
  fclose(fp);

  hammer_input input = hammer_input_new(buffer, fileSize);
  hammer_parser parser = hammer_seq(gzip_header_parser(), optional_fields_parser(), hammer_take(fileSize - 10), gzip_footer_parser(), hammer_end());
  hammer_result result = hammer_parse(parser, input);

  if (result.success) {
    printf("GZIP file parsed successfully!\n");
  } else {
    fprintf(stderr, "Error parsing GZIP file: %s at position %zu\n", result.error, result.position);
  }
  hammer_input_free(input);
  free(buffer);
  return 0;
}
