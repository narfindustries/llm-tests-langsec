#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

HParser* parse_AE;
HParser* parse_AS;
HParser* parse_AT;
HParser* parse_CS;
HParser* parse_DA;
HParser* parse_DS;
HParser* parse_DT;
HParser* parse_FL;
HParser* parse_FD;
HParser* parse_IS;
HParser* parse_LO;
HParser* parse_LT;
HParser* parse_OB;
HParser* parse_OD;
HParser* parse_OF;
HParser* parse_OW;
HParser* parse_PN;
HParser* parse_SH;
HParser* parse_SL;
HParser* parse_SQ;
HParser* parse_SS;
HParser* parse_ST;
HParser* parse_TM;
HParser* parse_UI;
HParser* parse_UL;
HParser* parse_UN;
HParser* parse_US;
HParser* parse_UT;

void init_parsers() {
    parse_AE = h_token((uint8_t*)"AE", 2);
    parse_AS = h_token((uint8_t*)"AS", 2);
    parse_AT = h_token((uint8_t*)"AT", 2);
    parse_CS = h_token((uint8_t*)"CS", 2);
    parse_DA = h_token((uint8_t*)"DA", 2);
    parse_DS = h_token((uint8_t*)"DS", 2);
    parse_DT = h_token((uint8_t*)"DT", 2);
    parse_FL = h_token((uint8_t*)"FL", 2);
    parse_FD = h_token((uint8_t*)"FD", 2);
    parse_IS = h_token((uint8_t*)"IS", 2);
    parse_LO = h_token((uint8_t*)"LO", 2);
    parse_LT = h_token((uint8_t*)"LT", 2);
    parse_OB = h_token((uint8_t*)"OB", 2);
    parse_OD = h_token((uint8_t*)"OD", 2);
    parse_OF = h_token((uint8_t*)"OF", 2);
    parse_OW = h_token((uint8_t*)"OW", 2);
    parse_PN = h_token((uint8_t*)"PN", 2);
    parse_SH = h_token((uint8_t*)"SH", 2);
    parse_SL = h_token((uint8_t*)"SL", 2);
    parse_SQ = h_token((uint8_t*)"SQ", 2);
    parse_SS = h_token((uint8_t*)"SS", 2);
    parse_ST = h_token((uint8_t*)"ST", 2);
    parse_TM = h_token((uint8_t*)"TM", 2);
    parse_UI = h_token((uint8_t*)"UI", 2);
    parse_UL = h_token((uint8_t*)"UL", 2);
    parse_UN = h_token((uint8_t*)"UN", 2);
    parse_US = h_token((uint8_t*)"US", 2);
    parse_UT = h_token((uint8_t*)"UT", 2);
}

HParser* parse_tag() {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* parse_value_length() {
    return h_uint32();
}

HParser* parse_element() {
    return h_sequence(
        parse_tag(),
        h_choice(parse_AE, parse_AS, parse_AT, parse_CS, parse_DA,
                parse_DS, parse_DT, parse_FL, parse_FD, parse_IS,
                parse_LO, parse_LT, parse_OB, parse_OD, parse_OF,
                parse_OW, parse_PN, parse_SH, parse_SL, parse_SQ,
                parse_SS, parse_ST, parse_TM, parse_UI, parse_UL,
                parse_UN, parse_US, parse_UT, NULL),
        parse_value_length(),
        h_many(h_uint8()),
        NULL
    );
}

HParser* parse_dataset() {
    return h_many(parse_element());
}

HParser* parse_file_meta() {
    return h_sequence(
        h_token((uint8_t*)"DICM", 4),
        parse_dataset(),
        NULL
    );
}

HParser* parse_dicom() {
    return h_sequence(
        h_token((uint8_t*)"\x00\x01", 128),
        parse_file_meta(),
        parse_dataset(),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    init_parsers();

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParseResult* result = h_parse(parse_dicom(), buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse DICOM file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}