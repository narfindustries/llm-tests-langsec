#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

static HParser mqtt_read_varint(HParser p) {
    return hmap(htake_while1(hsatisfy((HPredicate) [](u8 b){ return (b & 0x80) != 0;})), [](HResult r){
        u32 value = 0;
        u32 shift = 0;
        for (size_t i = 0; i < r.len; ++i) {
            value |= ((u32)r.data[i] & 0x7F) << shift;
            shift += 7;
        }
        return hresult_ok(value);
    });
}

static HParser mqtt_read_string(HParser p) {
    return hbind(mqtt_read_varint, [](HResult len_res, HParser p){
        if (hresult_is_err(len_res)) return len_res;
        u16 len = len_res.data;
        return hmap(htake(len), [](HResult r){
            char* str = (char*)malloc(len + 1);
            memcpy(str, r.data, len);
            str[len] = '\0';
            return hresult_ok(str);
        });
    });
}

static HParser mqtt_read_byte_array(HParser p){
    return hbind(mqtt_read_varint, [](HResult len_res, HParser p){
        if (hresult_is_err(len_res)) return len_res;
        u16 len = len_res.data;
        return hmap(htake(len), [](HResult r){
            u8* arr = (u8*)malloc(r.len);
            memcpy(arr, r.data, r.len);
            return hresult_ok(arr);
        });
    });
}

static HParser mqtt_connect_packet(HParser p) {
    return hseq(
        mqtt_read_string, 
        htake(1), 
        htake(1), 
        mqtt_read_varint, 
        mqtt_read_string, 
        hmaybe(mqtt_read_string), 
        hmaybe(mqtt_read_byte_array), 
        hmaybe(htake(1)), 
        hmaybe(htake(1)), 
        hmaybe(mqtt_read_string), 
        hmaybe(mqtt_read_byte_array), 
        [](HResult* results){
            //Process results here
            return hresult_ok(NULL);
        }
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    u8* buffer = (u8*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser input = hfrom_buf(buffer, fsize);
    HResult result = hparse(mqtt_connect_packet, input);

    if (hresult_is_ok(result)) {
        printf("MQTT packet parsed successfully!\n");
    } else {
        fprintf(stderr, "MQTT packet parsing failed: %s\n", hresult_get_error_msg(result));
    }

    free(buffer);
    return 0;
}
