#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

void init_parsers(HParser **gif_signature_parser, HParser **gif_version_parser, HParser **uint16_parser, HParser **uint8_parser, HParser **byte_parser, HParser **packed_fields_parser, HParser **global_color_table_flag_parser, HParser **color_resolution_parser, HParser **sort_flag_parser, HParser **size_of_global_color_table_parser, HParser **local_color_table_flag_parser, HParser **interlace_flag_parser, HParser **local_sort_flag_parser, HParser **size_of_local_color_table_parser, HParser **color_table_entry_parser, HParser **lzw_min_code_size_parser, HParser **image_data_sub_block_parser, HParser **image_data_parser, HParser **extension_introducer_parser, HParser **graphic_control_label_parser, HParser **plain_text_label_parser, HParser **application_extension_label_parser, HParser **comment_label_parser, HParser **graphic_control_extension_parser, HParser **plain_text_extension_parser, HParser **application_extension_parser, HParser **comment_extension_parser, HParser **extension_parser, HParser **gif_parser) {
    *gif_signature_parser = h_token("GIF", 3);
    *gif_version_parser = h_choice(h_token("87a", 3), h_token("89a", 3), NULL);
    *uint16_parser = h_uint16();
    *uint8_parser = h_uint8();
    *byte_parser = h_uint8();
    *packed_fields_parser = h_uint8();
    *global_color_table_flag_parser = h_bits(1, 0);
    *color_resolution_parser = h_bits(3, 1);
    *sort_flag_parser = h_bits(1, 4);
    *size_of_global_color_table_parser = h_bits(3, 5);
    *local_color_table_flag_parser = h_bits(1, 0);
    *interlace_flag_parser = h_bits(1, 1);
    *local_sort_flag_parser = h_bits(1, 2);
    *size_of_local_color_table_parser = h_bits(3, 5);
    *color_table_entry_parser = h_sequence(*uint8_parser, *uint8_parser, *uint8_parser, NULL);
    *lzw_min_code_size_parser = *uint8_parser;
    *image_data_sub_block_parser = h_sequence(*uint8_parser, h_repeat_n(*byte_parser, h_length_value(*uint8_parser, *uint8_parser)), NULL);
    *image_data_parser = h_sequence(*lzw_min_code_size_parser, h_repeat(*image_data_sub_block_parser), NULL);
    *extension_introducer_parser = h_token("\x21", 1);
    *graphic_control_label_parser = h_token("\xF9", 1);
    *plain_text_label_parser = h_token("\x01", 1);
    *application_extension_label_parser = h_token("\xFF", 1);
    *comment_label_parser = h_token("\xFE", 1);
    *graphic_control_extension_parser = h_sequence(*extension_introducer_parser, *graphic_control_label_parser, *uint8_parser, *packed_fields_parser, *uint16_parser, *uint8_parser, NULL);
    *plain_text_extension_parser = h_sequence(*extension_introducer_parser, *plain_text_label_parser, *uint8_parser, *uint16_parser, *uint16_parser, *uint16_parser, *uint16_parser, *uint8_parser, *uint8_parser, *uint8_parser, *uint8_parser, h_repeat(*image_data_sub_block_parser), NULL);
    *application_extension_parser = h_sequence(*extension_introducer_parser, *application_extension_label_parser, *uint8_parser, h_token("NETSCAPE", 8), h_token("2.0", 3), h_repeat(*image_data_sub_block_parser), NULL);
    *comment_extension_parser = h_sequence(*extension_introducer_parser, *comment_label_parser, h_repeat(*image_data_sub_block_parser), NULL);
    *extension_parser = h_choice(*graphic_control_extension_parser, *plain_text_extension_parser, *application_extension_parser, *comment_extension_parser, NULL);
    *gif_parser = h_sequence(*gif_signature_parser, *gif_version_parser, *uint16_parser, *uint16_parser, *packed_fields_parser, *uint8_parser, *uint8_parser, h_optional(h_repeat_n(*color_table_entry_parser, 1 << (h_length_value(*size_of_global_color_table_parser, *uint8_parser) + 1))), h_repeat(h_choice(h_sequence(h_token("\x2C", 1), *uint16_parser, *uint16_parser, *uint16_parser, *uint16_parser, *packed_fields_parser, h_optional(h_repeat_n(*color_table_entry_parser, 1 << (h_length_value(*size_of_local_color_table_parser, *uint8_parser) + 1))), *image_data_parser, NULL), *extension_parser, NULL)), h_token("\x3B", 1), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *gif_signature_parser, *gif_version_parser, *uint16_parser, *uint8_parser, *byte_parser, *packed_fields_parser, *global_color_table_flag_parser, *color_resolution_parser, *sort_flag_parser, *size_of_global_color_table_parser, *local_color_table_flag_parser, *interlace_flag_parser, *local_sort_flag_parser, *size_of_local_color_table_parser, *color_table_entry_parser, *lzw_min_code_size_parser, *image_data_sub_block_parser, *image_data_parser, *extension_introducer_parser, *graphic_control_label_parser, *plain_text_label_parser, *application_extension_label_parser, *comment_label_parser, *graphic_control_extension_parser, *plain_text_extension_parser, *application_extension_parser, *comment_extension_parser, *extension_parser, *gif_parser;

    init_parsers(&gif_signature_parser, &gif_version_parser, &uint16_parser, &uint8_parser, &byte_parser, &packed_fields_parser, &global_color_table_flag_parser, &color_resolution_parser, &sort_flag_parser, &size_of_global_color_table_parser, &local_color_table_flag_parser, &interlace_flag_parser, &local_sort_flag_parser, &size_of_local_color_table_parser, &color_table_entry_parser, &lzw_min_code_size_parser, &image_data_sub_block_parser, &image_data_parser, &extension_introducer_parser, &graphic_control_label_parser, &plain_text_label_parser, &application_extension_label_parser, &comment_label_parser, &graphic_control_extension_parser, &plain_text_extension_parser, &application_extension_parser, &comment_extension_parser, &extension_parser, &gif_parser);

    HParseResult *result = h_parse(gif_parser, data, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}