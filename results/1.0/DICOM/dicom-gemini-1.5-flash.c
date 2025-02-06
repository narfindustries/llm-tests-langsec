#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t group;
    uint16_t element;
    uint16_t vr_length;
    char vr[3]; 
    uint32_t length;
    unsigned char* value;
} dicom_attribute;

typedef struct {
    dicom_attribute* attributes;
    size_t attribute_count;
} dicom_file;

parser_t* parse_uint16() {
    return map(consume(2), (parse_fn) uint16_t_from_bytes);
}

parser_t* parse_uint32() {
    return map(consume(4), (parse_fn) uint32_t_from_bytes);
}

parser_t* parse_dicom_vr() {
  return map(consume(2),(parse_fn) string_from_bytes);
}

parser_t* parse_dicom_prefix() {
    return sequence(
        parse_uint16(),
        parse_uint16(),
        parse_uint16(),
        parse_dicom_vr(),
        parse_uint32(),
        (parse_fn) NULL
    );
}

parser_t* parse_dicom_attribute(void* file_ptr) {
    return sequence(
               parse_uint16(),
               parse_uint16(),
               parse_uint16(),
               parse_dicom_vr(),
               parse_uint32(),
               (parse_fn) NULL
           );
}

parser_t* parse_dicom_value(dicom_attribute* attr){
    return consume(attr->length);
}

parser_t* parse_dicom_element(void* file_ptr){
  return bind(parse_dicom_attribute, (parse_fn) append_dicom_attribute);
}


parser_t* parse_dicom_file() {
    return many(parse_dicom_element);
}

dicom_file* append_dicom_attribute(dicom_attribute* attr, void* file_ptr){
  dicom_file* file = (dicom_file*) file_ptr;
  if (file == NULL){
    file = (dicom_file*)malloc(sizeof(dicom_file));
    file->attributes = NULL;
    file->attribute_count = 0;
  }
  file->attributes = realloc(file->attributes,(file->attribute_count+1)*sizeof(dicom_attribute));
  memcpy(&file->attributes[file->attribute_count], attr, sizeof(dicom_attribute));
  file->attribute_count++;
  return file;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
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

    unsigned char* fileContent = (unsigned char*) malloc(fileSize);
    fread(fileContent, 1, fileSize, fp);
    fclose(fp);

    dicom_file* file = NULL;

    HParseResult result = h_parse(parse_dicom_file(), fileContent, fileSize, file);

    if(result.success){
      printf("Successfully parsed DICOM file\n");
      for(size_t i = 0; i< result.value->attribute_count; i++){
        printf("Group: %hu, Element: %hu, VR: %s, Length: %u\n",
                result.value->attributes[i].group, result.value->attributes[i].element,
                result.value->attributes[i].vr,result.value->attributes[i].length);
      }
    } else {
      printf("Failed to parse DICOM file at offset: %zu, error: %s\n", result.offset, result.error_message);
    }

    free(fileContent);
    if (result.value != NULL){
      free(result.value->attributes);
      free(result.value);
    }
    return 0;
}