#include <hammer/hammer.h>
#include <stdio.h>

// Helper parsers
static HParser* whitespace = h_in((const uint8_t*)" \t\n\r", 4);
static HParser* whitespace_ = h_many(whitespace);

// DICOM VR parsers
static HParser* application_entity = h_sequence(
    h_ch('A'), h_ch('E'),
    NULL
);

static HParser* age_string = h_sequence(
    h_ch('A'), h_ch('S'),
    NULL
);

static HParser* attribute_tag = h_sequence(
    h_ch('A'), h_ch('T'),
    NULL
);

static HParser* code_string = h_sequence(
    h_ch('C'), h_ch('S'),
    NULL
);

static HParser* date = h_sequence(
    h_ch('D'), h_ch('A'),
    NULL
);

static HParser* decimal_string = h_sequence(
    h_ch('D'), h_ch('S'),
    NULL
);

static HParser* datetime = h_sequence(
    h_ch('D'), h_ch('T'),
    NULL
);

static HParser* floating_point_single = h_sequence(
    h_ch('F'), h_ch('L'),
    NULL
);

static HParser* floating_point_double = h_sequence(
    h_ch('F'), h_ch('D'),
    NULL
);

static HParser* integer_string = h_sequence(
    h_ch('I'), h_ch('S'),
    NULL
);

static HParser* long_string = h_sequence(
    h_ch('L'), h_ch('O'),
    NULL
);

static HParser* long_text = h_sequence(
    h_ch('L'), h_ch('T'),
    NULL
);

static HParser* other_byte = h_sequence(
    h_ch('O'), h_ch('B'),
    NULL
);

static HParser* other_double = h_sequence(
    h_ch('O'), h_ch('D'),
    NULL
);

static HParser* other_float = h_sequence(
    h_ch('O'), h_ch('F'),
    NULL
);

static HParser* other_long = h_sequence(
    h_ch('O'), h_ch('L'),
    NULL
);

static HParser* other_word = h_sequence(
    h_ch('O'), h_ch('W'),
    NULL
);

static HParser* person_name = h_sequence(
    h_ch('P'), h_ch('N'),
    NULL
);

static HParser* short_string = h_sequence(
    h_ch('S'), h_ch('H'),
    NULL
);

static HParser* signed_long = h_sequence(
    h_ch('S'), h_ch('L'),
    NULL
);

static HParser* sequence_of_items = h_sequence(
    h_ch('S'), h_ch('Q'),
    NULL
);

static HParser* signed_short = h_sequence(
    h_ch('S'), h_ch('S'),
    NULL
);

static HParser* short_text = h_sequence(
    h_ch('S'), h_ch('T'),
    NULL
);

static HParser* time = h_sequence(
    h_ch('T'), h_ch('M'),
    NULL
);

static HParser* unlimited_characters = h_sequence(
    h_ch('U'), h_ch('C'),
    NULL
);

static HParser* unique_identifier = h_sequence(
    h_ch('U'), h_ch('I'),
    NULL
);

static HParser* unsigned_long = h_sequence(
    h_ch('U'), h_ch('L'),
    NULL
);

static HParser* unknown = h_sequence(
    h_ch('U'), h_ch('N'),
    NULL
);

static HParser* unsigned_short = h_sequence(
    h_ch('U'), h_ch('S'),
    NULL
);

static HParser* unlimited_text = h_sequence(
    h_ch('U'), h_ch('T'),
    NULL
);

// Main VR parser combining all VR types
static HParser* vr_type = h_choice(
    application_entity, age_string, attribute_tag,
    code_string, date, decimal_string, datetime,
    floating_point_single, floating_point_double,
    integer_string, long_string, long_text,
    other_byte, other_double, other_float,
    other_long, other_word, person_name,
    short_string, signed_long, sequence_of_items,
    signed_short, short_text, time,
    unlimited_characters, unique_identifier,
    unsigned_long, unknown, unsigned_short,
    unlimited_text,
    NULL
);

// Main parser for complete DICOM tag
static HParser* dicom_tag = h_sequence(
    h_ch('('),
    h_repeat_n(h_hex_digit(), 4),
    h_ch(','),
    h_repeat_n(h_hex_digit(), 4),
    h_ch(')'),
    whitespace_,
    vr_type,
    NULL
);

// Parse function
void parse_dicom(const char* input) {
    HParseResult* result = h_parse(dicom_tag, (const uint8_t*)input, strlen(input));
    if(result) {
        printf("Successfully parsed DICOM tag\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse DICOM tag\n");
    }
}

int main() {
    parse_dicom("(0010,0020) PN");
    return 0;
}