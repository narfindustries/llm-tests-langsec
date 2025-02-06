#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAMMERInsensitive 1

void hammer_token(int a, char *b, char *c) {}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *data = malloc(file_size);
    if (!data) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Failed to read file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    char *id = "[a-zA-Z][a-zA-Z0-9-]*";
    char *token = "[!#$%&'*+.^_`|~0-9a-zA-Z-]+";
    char *separators = "[ ()<>@,;:\\\"/\\[\\]\\?={}\t ]+";
    char *comment = "#.*\n";
    char *quoted_string = "\"[^\"]*\"";
    char *request_line = "(GET|HEAD|POST|PUT|DELETE|OPTIONS|TRACE|CONNECT|PATCH) .*";
    char *request_target = "^[^ ]+|\\*";
    char *http_version = "HTTP/1\\.1";
    char *request = strcat(strdup(request_line), "\\r\\n");
    char *field_name = id;
    char *field_value = "[^\r\n]*";
    char *message_header = strcat(strdup(field_name), " : ");
    message_header = strcat(message_header, field_value);
    message_header = strcat(message_header, "\\r\\n");
    char *headers = strcat(strdup(message_header), "*");
    char *status_code = "^[0-9]{3}";
    char *reason_phrase = "[^\r\n]*";
    char *status_line = strcat(strdup(http_version), " ");
    status_line = strcat(status_line, status_code);
    status_line = strcat(status_line, " ");
    status_line = strcat(status_line, reason_phrase);
    status_line = strcat(status_line, "\\r\\n");
    char *response = strcat(strdup(status_line), headers);
    char *entity_body = ".*";
    char *http_message = strcat(strdup(request), " ");
    http_message = strcat(http_message, headers);
    http_message = strcat(http_message, " ");
    http_message = strcat(http_message, entity_body);
    http_message = strcat(http_message, " | ");
    http_message = strcat(http_message, response);
    http_message = strcat(http_message, " ");
    http_message = strcat(http_message, entity_body);

    printf("%s\n", id);
    printf("%s\n", token);
    printf("%s\n", separators);
    printf("%s\n", comment);
    printf("%s\n", quoted_string);
    printf("%s\n", request_line);
    printf("%s\n", request_target);
    printf("%s\n", http_version);
    printf("%s\n", request);
    printf("%s\n", field_name);
    printf("%s\n", field_value);
    printf("%s\n", message_header);
    printf("%s\n", headers);
    printf("%s\n", status_code);
    printf("%s\n", reason_phrase);
    printf("%s\n", status_line);
    printf("%s\n", response);
    printf("%s\n", entity_body);
    printf("%s\n", http_message);

    free(data);

    return 0;
}