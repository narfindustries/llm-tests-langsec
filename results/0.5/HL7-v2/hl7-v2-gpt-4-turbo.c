#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define basic field types
static HParser *adt;
static HParser *msg_type;
static HParser *msg_event;
static HParser *msg_struct_id;
static HParser *field_separator;
static HParser *encoding_chars;
static HParser *sending_app;
static HParser *sending_fac;
static HParser *receiving_app;
static HParser *receiving_fac;
static HParser *date_time;
static HParser *security;
static HParser *message_type;
static HParser *message_control_id;
static HParser *processing_id;
static HParser *version_id;
static HParser *segment;
static HParser *segments;

// HL7 message segments
static HParser *msh_segment;
static HParser *evn_segment;
static HParser *pid_segment;
static HParser *hl7_message;

// Helper parsers
static HParser *field;
static HParser *component;
static HParser *subcomponent;

// Initialize parsers for basic field types
static void init_field_types() {
    adt = h_token("ADT", 3);
    msg_type = h_ch('A');
    msg_event = h_ch('O');
    msg_struct_id = h_sequence(h_ch('A'), h_ch('D'), h_ch('T'), NULL);
    field_separator = h_ch('|');
    encoding_chars = h_ch('^');
    sending_app = h_token("APP", 3);
    sending_fac = h_token("FAC", 3);
    receiving_app = h_token("APP", 3);
    receiving_fac = h_token("FAC", 3);
    date_time = h_token("20231210", 8);
    security = h_token("SEC", 3);
    message_type = h_sequence(msg_type, h_ch('_'), msg_event, NULL);
    message_control_id = h_token("123456", 6);
    processing_id = h_token("P", 1);
    version_id = h_token("2.5", 3);
}

// Initialize parsers for segments
static void init_segments() {
    msh_segment = h_sequence(
        h_ch('M'), h_ch('S'), h_ch('H'),
        field_separator,
        encoding_chars,
        field_separator,
        sending_app,
        field_separator,
        sending_fac,
        field_separator,
        receiving_app,
        field_separator,
        receiving_fac,
        field_separator,
        date_time,
        field_separator,
        security,
        field_separator,
        message_type,
        field_separator,
        message_control_id,
        field_separator,
        processing_id,
        field_separator,
        version_id,
        NULL
    );

    evn_segment = h_sequence(
        h_ch('E'), h_ch('V'), h_ch('N'),
        field_separator,
        encoding_chars,
        field_separator,
        date_time,
        field_separator,
        NULL
    );

    pid_segment = h_sequence(
        h_ch('P'), h_ch('I'), h_ch('D'),
        field_separator,
        NULL
    );

    segments = h_many(h_choice(msh_segment, evn_segment, pid_segment, NULL));
}

// Initialize the HL7 message parser
static void init_hl7_message() {
    hl7_message = h_sequence(segments, NULL);
}

// Initialize all parsers
static void init_parsers() {
    init_field_types();
    init_segments();
    init_hl7_message();
}

int main(int argc, char **argv) {
    init_parsers();
    HParser *parser = hl7_message;
    HParseResult *result = h_parse(parser, (const uint8_t *)"MSH|^~\\&|APP|FAC|APP|FAC|20231210|SEC|A_O|123456|P|2.5", 53);
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed!\n");
    }
    return 0;
}