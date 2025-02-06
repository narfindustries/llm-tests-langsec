#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FIELD_DELIMITER '|'
#define COMPONENT_DELIMITER '^'
#define SUBCOMPONENT_DELIMITER '&'

void init_parsers(HParser **hl7_char, HParser **field_separator, HParser **component_separator, HParser **subcomponent_separator, 
                  HParser **hl7_field, HParser **hl7_component, HParser **hl7_subcomponent, 
                  HParser **msh_segment, HParser **pid_segment, HParser **pv1_segment, HParser **obr_segment, HParser **obx_segment, 
                  HParser **hl7_message) {
    *hl7_char = h_ch_range(0x20, 0x7E); // Printable ASCII characters
    *field_separator = h_ch(FIELD_DELIMITER);
    *component_separator = h_ch(COMPONENT_DELIMITER);
    *subcomponent_separator = h_ch(SUBCOMPONENT_DELIMITER);
    *hl7_field = h_many1(*hl7_char);
    *hl7_component = h_many1(*hl7_char);
    *hl7_subcomponent = h_many1(*hl7_char);
    *msh_segment = h_sequence(h_ch('M'), h_ch('S'), h_ch('H'), *field_separator, NULL);
    *pid_segment = h_sequence(h_ch('P'), h_ch('I'), h_ch('D'), *field_separator, NULL);
    *pv1_segment = h_sequence(h_ch('P'), h_ch('V'), h_ch('1'), *field_separator, NULL);
    *obr_segment = h_sequence(h_ch('O'), h_ch('B'), h_ch('R'), *field_separator, NULL);
    *obx_segment = h_sequence(h_ch('O'), h_ch('B'), h_ch('X'), *field_separator, NULL);
    *hl7_message = h_many1(h_choice(*msh_segment, *pid_segment, *pv1_segment, *obr_segment, *obx_segment, NULL));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *hl7_char, *field_separator, *component_separator, *subcomponent_separator;
    HParser *hl7_field, *hl7_component, *hl7_subcomponent;
    HParser *msh_segment, *pid_segment, *pv1_segment, *obr_segment, *obx_segment;
    HParser *hl7_message;

    init_parsers(&hl7_char, &field_separator, &component_separator, &subcomponent_separator, 
                 &hl7_field, &hl7_component, &hl7_subcomponent, 
                 &msh_segment, &pid_segment, &pv1_segment, &obr_segment, &obx_segment, 
                 &hl7_message);

    HParseResult *result = h_parse(hl7_message, data, file_size);
    if (result) {
        printf("Parsed HL7 message successfully.\n");
    } else {
        printf("Failed to parse HL7 message.\n");
    }

    free(data);
    return 0;
}