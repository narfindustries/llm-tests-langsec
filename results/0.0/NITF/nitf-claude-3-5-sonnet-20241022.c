#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t nitf_header[] = {0x4E, 0x49, 0x54, 0x46};

HParser* init_nitf_parser() {
    HParser* file_header = h_token(nitf_header, sizeof(nitf_header));
    HParser* file_version = h_token((const uint8_t*)"02.10", 5);
    HParser* complexity_level = h_token((const uint8_t*)"03", 2);
    HParser* standard_type = h_token((const uint8_t*)"BF01", 4);
    HParser* originating_station = h_repeat_n(h_ch_range(0x20, 0x7E), 10);
    HParser* date_time = h_repeat_n(h_ch_range(0x30, 0x39), 14);
    HParser* file_title = h_repeat_n(h_ch_range(0x20, 0x7E), 80);
    HParser* security_class = h_ch_range('A', 'Z');
    HParser* message_copy = h_ch_range('0', '9');
    HParser* encryption = h_ch_range('0', '9');
    HParser* background_color = h_repeat_n(h_ch_range(0x30, 0x39), 3);
    HParser* originator_name = h_repeat_n(h_ch_range(0x20, 0x7E), 24);
    HParser* originator_phone = h_repeat_n(h_ch_range(0x20, 0x7E), 18);

    return h_sequence(file_header,
                     file_version,
                     complexity_level,
                     standard_type,
                     originating_station,
                     date_time,
                     file_title,
                     security_class,
                     message_copy,
                     encryption,
                     background_color,
                     originator_name,
                     originator_phone,
                     NULL);
}

int main(int argc, char* argv[]) {
    HParser* parser = init_nitf_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    const uint8_t input[] = "NITF02.1003BF01Station   202310221200Test File Title                                                                    U0000000000000John Doe                 555-123-4567       ";
    size_t input_len = sizeof(input) - 1;

    HParseResult* result = h_parse(parser, input, input_len);
    
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    h_parse_result_free(result);
    return 0;
}