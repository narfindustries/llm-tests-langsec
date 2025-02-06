#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

void init_parsers(HParser **byte_parser, HParser **uint16_parser, HParser **uint32_parser, HParser **gif_header_parser, HParser **logical_screen_descriptor_parser, HParser **global_color_table_parser, HParser **image_descriptor_parser, HParser **local_color_table_parser, HParser **image_data_parser, HParser **graphic_control_extension_parser, HParser **plain_text_extension_parser, HParser **application_extension_parser, HParser **comment_extension_parser, HParser **gif_trailer_parser, HParser **gif_parser) {
    *byte_parser = h_uint8();
    *uint16_parser = h_uint16();
    *uint32_parser = h_uint32();
    *gif_header_parser = h_sequence(h_token("GIF", 3), h_choice(h_token("87a", 3), h_token("89a", 3), NULL), NULL);
    *logical_screen_descriptor_parser = h_sequence(*uint16_parser, *uint16_parser, *byte_parser, *byte_parser, *byte_parser, NULL);
    *global_color_table_parser = h_many(h_sequence(*byte_parser, *byte_parser, *byte_parser, NULL));
    *image_descriptor_parser = h_sequence(*byte_parser, *uint16_parser, *uint16_parser, *uint16_parser, *uint16_parser, *byte_parser, NULL);
    *local_color_table_parser = h_many(h_sequence(*byte_parser, *byte_parser, *byte_parser, NULL));
    *image_data_parser = h_sequence(*byte_parser, h_many1(h_sequence(*byte_parser, h_many(*byte_parser), NULL)), NULL);
    *graphic_control_extension_parser = h_sequence(*byte_parser, *byte_parser, *byte_parser, *byte_parser, *uint16_parser, *byte_parser, *byte_parser, NULL);
    *plain_text_extension_parser = h_sequence(*byte_parser, *byte_parser, *byte_parser, *uint16_parser, *uint16_parser, *uint16_parser, *uint16_parser, *byte_parser, *byte_parser, *byte_parser, *byte_parser, h_many1(h_sequence(*byte_parser, h_many(*byte_parser), NULL)), *byte_parser, NULL);
    *application_extension_parser = h_sequence(*byte_parser, *byte_parser, *byte_parser, h_token("NETSCAPE", 8), h_token("2.0", 3), h_many1(h_sequence(*byte_parser, h_many(*byte_parser), NULL)), *byte_parser, NULL);
    *comment_extension_parser = h_sequence(*byte_parser, *byte_parser, h_many1(h_sequence(*byte_parser, h_many(*byte_parser), NULL)), *byte_parser, NULL);
    *gif_trailer_parser = h_sequence(*byte_parser, NULL);
    *gif_parser = h_sequence(*gif_header_parser, *logical_screen_descriptor_parser, h_optional(*global_color_table_parser), h_many(h_choice(*image_descriptor_parser, h_sequence(*local_color_table_parser, *image_data_parser, NULL), *graphic_control_extension_parser, *plain_text_extension_parser, *application_extension_parser, *comment_extension_parser, NULL)), *gif_trailer_parser, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
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

    HParser *byte_parser, *uint16_parser, *uint32_parser, *gif_header_parser, *logical_screen_descriptor_parser, *global_color_table_parser, *image_descriptor_parser, *local_color_table_parser, *image_data_parser, *graphic_control_extension_parser, *plain_text_extension_parser, *application_extension_parser, *comment_extension_parser, *gif_trailer_parser, *gif_parser;
    init_parsers(&byte_parser, &uint16_parser, &uint32_parser, &gif_header_parser, &logical_screen_descriptor_parser, &global_color_table_parser, &image_descriptor_parser, &local_color_table_parser, &image_data_parser, &graphic_control_extension_parser, &plain_text_extension_parser, &application_extension_parser, &comment_extension_parser, &gif_trailer_parser, &gif_parser);

    HParseResult *result = h_parse(gif_parser, data, file_size);
    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}