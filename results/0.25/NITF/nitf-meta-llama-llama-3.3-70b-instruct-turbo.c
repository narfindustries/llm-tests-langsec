#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the NITF header structure
typedef struct {
    char file_header[4];
    uint32_t file_header_length;
    char header_type[25];
    uint16_t header_version;
    uint32_t file_time;
    char classification[5];
    char title[80];
    char security_control_info[11];
    char releasing_instruction[20];
    char file_description[80];
    char file_title[80];
    char file_security_date[11];
    char file_downgrade[2];
    char file_downgrade_to[10];
    char file_classification_system[11];
    char file_control_and_handle[15];
    char file_name[24];
} nitf_header_t;

// Define the LLAMA header structure
typedef struct {
    char header_type[4];
    uint32_t header_length;
    uint16_t version;
    uint16_t turbo_instruct;
} llama_header_t;

// Define the NITF meta section structure
typedef struct {
    char section_type[4];
    uint32_t section_length;
    nitf_header_t nitf_header;
    llama_header_t llama_header;
} nitf_meta_t;

// Define the output structure
typedef struct {
    nitf_meta_t nitf_meta;
    char output[1024];
} output_t;

int main() {
    // Initialize the output structure
    output_t output;
    memset(&output, 0, sizeof(output_t));

    // Set the NITF header values
    memcpy(output.nitf_meta.nitf_header.file_header, "NITF", 4);
    output.nitf_meta.nitf_header.file_header_length = 0x00000138;
    memcpy(output.nitf_meta.nitf_header.header_type, "LLAMA", 5);
    output.nitf_meta.nitf_header.header_version = 0x0303;
    output.nitf_meta.nitf_header.file_time = 0x12345678;

    // Set the LLAMA header values
    memcpy(output.nitf_meta.llama_header.header_type, "LLMA", 4);
    output.nitf_meta.llama_header.header_length = 0x00000010;
    output.nitf_meta.llama_header.version = 0x0303;
    output.nitf_meta.llama_header.turbo_instruct = 0x0001;

    // Compile and execute the output
    FILE *fp = fopen("output_hammer/nitf-meta-llama-llama-3/output", "wb");
    if (fp == NULL) {
        printf("Error opening file for writing\n");
        return 1;
    }
    fwrite(&output, sizeof(output_t), 1, fp);
    fclose(fp);

    return 0;
}