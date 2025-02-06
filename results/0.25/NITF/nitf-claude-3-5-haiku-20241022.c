#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char fhdr[10];
    int clevel;
    char stype;
    char ostaid[11];
    char fdt[15];
    char ftitle[81];
    
    char classification[3];
    char security_group[21];
    char release_info[21];
    char declassification_date[9];
    char declassification_type[3];
    
    char isorce[43];
    char idatim[15];
    int iclevel;
    char encryp;
    char icompress[4];
} NitfFile;

HParser* nitf_parser() {
    HParser* fhdr = h_token_c_string("NITF02.10");
    HParser* clevel = h_int_range(h_int_range(h_ch('0'), h_ch('9')), 3, 7);
    HParser* stype = h_choice(
        h_ch('C'),
        h_ch('R'),
        h_ch('U'),
        NULL
    );
    HParser* ostaid = h_repeat_n(h_ch_range('A', 'Z'), 10);
    HParser* fdt = h_repeat_n(h_ch_range('0', '9'), 14);
    HParser* ftitle = h_repeat_n(h_ch_range(32, 126), 80);
    
    HParser* nitf_header = h_sequence(
        fhdr, clevel, stype, ostaid, fdt, ftitle, 
        NULL
    );
    
    return nitf_header;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <nitf_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);
    
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }
    
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);
    
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }
    
    HParser* parser = nitf_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);
    
    if (result && result->ast) {
        printf("NITF file parsed successfully\n");
    } else {
        printf("NITF file parsing failed\n");
    }
    
    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);
    
    return 0;
}