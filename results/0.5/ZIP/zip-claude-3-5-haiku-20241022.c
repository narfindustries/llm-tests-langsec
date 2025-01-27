#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser* zip_parser(void) {
    HParser* digit = h_ch_range('0', '9');
    HParser* digits = h_many(digit);
    
    HParser* letter = h_ch_range('a', 'z');
    HParser* letters = h_many(letter);
    
    HParser* filename = h_sequence(letters, h_ch('.'), letters, NULL);
    
    HParser* file_entry = h_sequence(
        filename,
        h_ch(' '),
        digits,
        h_ch(' '),
        h_ch_range('0', '9'),
        h_ch_range('0', '9'),
        h_ch_range('0', '9'),
        h_ch_range('0', '9'),
        NULL
    );
    
    HParser* zip_contents = h_many(file_entry);
    
    return zip_contents;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);
    
    char* buffer = malloc(file_size + 1);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }
    
    size_t read_size = fread(buffer, 1, file_size, file);
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    buffer[file_size] = '\0';
    
    HParser* parser = zip_parser();
    HParseResult* result = h_parse(parser, (uint8_t*)buffer, read_size);
    
    if (result && result->ast) {
        printf("Parsing successful\n");
    } else {
        printf("Parsing failed\n");
    }
    
    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);
    fclose(file);
    
    return 0;
}