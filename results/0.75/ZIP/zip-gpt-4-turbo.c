#include <stdio.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Parsing specifications for a ZIP file using the Hammer parsing library.

static HParsedToken *parse_dos_time(const HParseResult *p) {
    uint16_t dos_time = H_CAST_UINT(p);
    uint16_t hour = (dos_time >> 11) & 0x1F;
    uint16_t minute = (dos_time >> 5) & 0x3F;
    uint16_t second = (dos_time & 0x1F) * 2;

    H_ALLOC_TOKEN_UINT(t_hour, hour);
    H_ALLOC_TOKEN_UINT(t_minute, minute);
    H_ALLOC_TOKEN_UINT(t_second, second);

    const HParsedToken *t_time[] = {t_hour, t_minute, t_second};
    return H_MAKE_SEQ(t_time, 3);
}

static HParsedToken *parse_dos_date(const HParseResult *p) {
    uint16_t dos_date = H_CAST_UINT(p);
    uint16_t year = ((dos_date >> 9) & 0x7F) + 1980;
    uint16_t month = (dos_date >> 5) & 0x0F;
    uint16_t day = dos_date & 0x1F;

    H_ALLOC_TOKEN_UINT(t_year, year);
    H_ALLOC_TOKEN_UINT(t_month, month);
    H_ALLOC_TOKEN_UINT(t_day, day);

    const HParsedToken *t_date[] = {t_year, t_month, t_day};
    return H_MAKE_SEQ(t_date, 3);
}

static const HParser *bit16 = h_uint16_le();
static const HParser *bit32 = h_uint32_le();

static const HParser *dos_time = h_action(bit16, parse_dos_time, NULL);
static const HParser *dos_date = h_action(bit16, parse_dos_date, NULL);

static const HParser *zip_file_header = h_sequence(
    h_bytes_lit("PK\003\004", 4),   // Signature
    h_uint16_le(),                  // Version needed to extract
    h_uint16_le(),                  // General purpose bit flag
    h_uint16_le(),                  // Compression method
    dos_time,                       // Last mod file time
    dos_date,                       // Last mod file date
    h_uint32_le(),                  // CRC-32
    h_uint32_le(),                  // Compressed size
    h_uint32_le(),                  // Uncompressed size
    h_uint16_le(),                  // File name length
    h_uint16_le(),                  // Extra field length
    h_bind(h_uint16_le(),           // Length of file name
        h_length_value(h_uint16_le(), h_bytes)), // File name
    h_bind(h_uint16_le(),           // Length of extra field
        h_length_value(h_uint16_le(), h_bytes)),  // Extra field
    NULL
);

static const HParser *zip_file = h_sequence(
    zip_file_header,
    h_many(h_ch('a')),              // Placeholder for file content
    NULL
);

int main(int argc, char **argv) {
    HParseResult *result = h_parse(zip_file, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse.\n");
    }
    return 0;
}