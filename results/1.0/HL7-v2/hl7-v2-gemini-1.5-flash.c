#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

//Simplified HL7 v2 parser using Hammer -  still does NOT cover the entire specification

typedef struct HParser_ HParser;
typedef struct HParseResult_ HParseResult;
typedef struct HArena_ HArena;
typedef struct h_list_ HList;


HParser* parse_string(const char* str) {
    return h_string(str);
}

HParser* parse_component(void) {
    return h_regex("[^\\|]+");
}

HParser* parse_field(void) {
    return h_sepBy1(parse_component(), h_ch('|'));
}

HParser* parse_segment(const char* segment_name) {
    HParser* segment_parser = h_seq(
        h_string(segment_name),
        h_ch('^'),
        h_many1(parse_field()), 
        h_ch('\r'),
        h_ch('\n')
    );
    return segment_parser;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buffer = (char*)malloc(fsize + 1);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    buffer[fsize] = 0; 
    fclose(fp);

    HArena* arena = h_arena_new();
    HParser* msh_parser = parse_segment("MSH");
    HParseResult* msh_result = h_parse(msh_parser, (const uint8_t*)buffer, strlen(buffer), arena);


    if (msh_result->success) {
        printf("MSH segment parsed successfully.\n");
        HList* fields = (HList*)msh_result->value;
        for (size_t i = 0; i < fields->size; i++) {
            HList* components = (HList*)h_list_get(fields, i);
            for (size_t j = 0; j < components->size; j++) {
                printf("Field %zu, Component %zu: %s\n", i + 1, j + 1, (char*)h_list_get(components, j));
            }
        }
        h_list_free(fields);

    } else {
        fprintf(stderr, "MSH segment parsing failed: %s\n", msh_result->error);
    }

    free(buffer);
    h_arena_free(arena);
    return 0;
}
