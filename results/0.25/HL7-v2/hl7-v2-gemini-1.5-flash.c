#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* value;
} HL7String;

typedef struct {
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
} HL7DateTime;

hammer_parser HL7String_parser() {
    return hammer_string();
}

hammer_parser HL7DateTime_parser() {
    return hammer_map(
        hammer_seq(
            hammer_int(),
            hammer_char('-'),
            hammer_int(),
            hammer_char('-'),
            hammer_int(),
            hammer_char(' '),
            hammer_int(),
            hammer_char(':'),
            hammer_int(),
            hammer_char(':'),
            hammer_int()
        ),
        (void*) [](hammer_tuple t) {
            HL7DateTime* dt = malloc(sizeof(HL7DateTime));
            dt->year = hammer_get_int(hammer_get_element(t, 0));
            dt->month = hammer_get_int(hammer_get_element(t, 2));
            dt->day = hammer_get_int(hammer_get_element(t, 4));
            dt->hour = hammer_get_int(hammer_get_element(t, 6));
            dt->minute = hammer_get_int(hammer_get_element(t, 8));
            dt->second = hammer_get_int(hammer_get_element(t, 10));
            return dt;
        }
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* fileContent = (char*) malloc(fileSize + 1);
    fread(fileContent, 1, fileSize, fp);
    fileContent[fileSize] = '\0';
    fclose(fp);

    hammer_parser mshParser = hammer_seq(
        HL7String_parser(), 
        hammer_char('|'),
        HL7String_parser(), 
        hammer_char('|'),
        HL7String_parser(), 
        hammer_char('|'),
        HL7String_parser(), 
        hammer_char('|'),
        HL7String_parser(), 
        hammer_char('|'),
        HL7String_parser(), 
        hammer_char('|'),
        HL7DateTime_parser(), 
        hammer_char('|'),
        HL7String_parser()
    );

    hammer_result result = hammer_parse(mshParser, fileContent);

    if (hammer_is_success(result)) {
        printf("Parsing successful!\n");
        HL7String* sendingApplication = hammer_get_value(hammer_get_element(hammer_get_value(result), 2));
        printf("Sending Application: %s\n", sendingApplication->value);
        free(sendingApplication->value);
        free(sendingApplication);
    } else {
        printf("Parsing failed: %s\n", hammer_get_error(result));
    }

    hammer_free_result(result);
    free(fileContent);
    return 0;
}
