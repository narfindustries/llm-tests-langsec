#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t nitf_header[] = "NITF";

HParser* init_nitf_parser() {
    // Basic building blocks
    HParser* file_header = h_token(nitf_header, sizeof(nitf_header)-1);
    HParser* file_version = h_token((const uint8_t*)"02.10", 5);
    
    // Complex length fields
    HParser* length_2 = h_repeat_n(h_ch_range(0x30, 0x39), 2);
    HParser* length_4 = h_repeat_n(h_ch_range(0x30, 0x39), 4);
    HParser* length_5 = h_repeat_n(h_ch_range(0x30, 0x39), 5);
    HParser* length_6 = h_repeat_n(h_ch_range(0x30, 0x39), 6);
    HParser* length_7 = h_repeat_n(h_ch_range(0x30, 0x39), 7);
    HParser* length_10 = h_repeat_n(h_ch_range(0x30, 0x39), 10);
    HParser* length_12 = h_repeat_n(h_ch_range(0x30, 0x39), 12);
    HParser* length_14 = h_repeat_n(h_ch_range(0x30, 0x39), 14);

    // Date and time fields
    HParser* date_time = h_repeat_n(h_ch_range(0x30, 0x39), 14);
    
    // Security fields
    HParser* security_field = h_repeat_n(h_ch_range(0x20, 0x7E), 40);
    
    // File header components
    HParser* complexity_level = length_2;
    HParser* standard_type = h_token((const uint8_t*)"BF01", 4);
    HParser* originating_station = h_repeat_n(h_ch_range(0x20, 0x7E), 10);
    HParser* file_date_time = date_time;
    HParser* file_title = h_repeat_n(h_ch_range(0x20, 0x7E), 80);
    
    // Security group
    HParser* security_classification = h_ch_range(0x20, 0x7E);
    HParser* security_system = security_field;
    HParser* codewords = security_field;
    HParser* control_and_handling = security_field;
    HParser* release_instructions = security_field;
    HParser* declass_type = security_field;
    HParser* declass_date = h_repeat_n(h_ch_range(0x20, 0x7E), 8);
    HParser* declass_exemption = h_repeat_n(h_ch_range(0x20, 0x7E), 4);
    HParser* downgrade = h_ch_range(0x20, 0x7E);
    HParser* downgrade_date = h_repeat_n(h_ch_range(0x20, 0x7E), 8);
    HParser* classification_text = h_repeat_n(h_ch_range(0x20, 0x7E), 43);
    HParser* classification_authority_type = h_ch_range(0x20, 0x7E);
    HParser* classification_authority = h_repeat_n(h_ch_range(0x20, 0x7E), 40);
    HParser* classification_reason = h_repeat_n(h_ch_range(0x20, 0x7E), 1);
    HParser* security_source_date = h_repeat_n(h_ch_range(0x20, 0x7E), 8);
    HParser* security_control_number = h_repeat_n(h_ch_range(0x20, 0x7E), 15);
    
    // Background color
    HParser* background_color = h_repeat_n(h_ch_range(0x30, 0x39), 3);
    
    // Originator name and phone
    HParser* originator_name = h_repeat_n(h_ch_range(0x20, 0x7E), 24);
    HParser* originator_phone = h_repeat_n(h_ch_range(0x20, 0x7E), 18);
    
    // File length fields
    HParser* file_length = length_12;
    HParser* header_length = length_6;
    
    // Image segments
    HParser* image_segments = length_3;
    HParser* symbol_segments = length_3;
    HParser* label_segments = length_3;
    HParser* text_segments = length_3;
    HParser* data_extension_segments = length_3;
    HParser* reserved_extension_segments = length_3;
    
    // User defined header data
    HParser* user_defined_header_length = length_5;
    HParser* extended_header_length = length_5;

    // Combine all parsers
    return h_sequence(file_header,
                     file_version,
                     complexity_level,
                     standard_type,
                     originating_station,
                     file_date_time,
                     file_title,
                     security_classification,
                     security_system,
                     codewords,
                     control_and_handling,
                     release_instructions,
                     declass_type,
                     declass_date,
                     declass_exemption,
                     downgrade,
                     downgrade_date,
                     classification_text,
                     classification_authority_type,
                     classification_authority,
                     classification_reason,
                     security_source_date,
                     security_control_number,
                     background_color,
                     originator_name,
                     originator_phone,
                     file_length,
                     header_length,
                     image_segments,
                     symbol_segments,
                     label_segments, 
                     text_segments,
                     data_extension_segments,
                     reserved_extension_segments,
                     user_defined_header_length,
                     extended_header_length,
                     NULL);
}

int main() {
    HParser* parser = init_nitf_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}