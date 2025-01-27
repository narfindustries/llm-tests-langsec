#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <hammer/parser.h>
#include <hammer/stream.h>
#include <hammer/allocator.h>
#include <hammer/error.h>
#include <hammer/input.h>
#include <hammer/output.h>
#include <hammer/stream.h>
#include <hammer/stream_buffer.h>
#include <hammer/stream_file.h>
#include <hammer/stream_memory.h>
#include <hammer/stream_string.h>
#include <hammer/stream_vector.h>
#include <hammer/stream_view.h>
#include <hammer/stream_window.h>
#include <hammer/stream_zip.h>
#include <hammer/stream_zip_entry.h>
#include <hammer/stream_zip_file.h>
#include <hammer/stream_zip_memory.h>
#include <hammer/stream_zip_string.h>
#include <hammer/stream_zip_vector.h>
#include <hammer/stream_zip_view.h>
#include <hammer/stream_zip_window.h>

H_PARSER(parse_zip) {
    return h_sequence(
        h_magic("\x50\x4B\x03\x04", 4), // ZIP file signature
        h_uint16_t(), // Version needed to extract
        h_uint16_t(), // General purpose bit flag
        h_uint16_t(), // Compression method
        h_uint16_t(), // Last mod file time
        h_uint16_t(), // Last mod file date
        h_uint32_t(), // CRC-32
        h_uint32_t(), // Compressed size
        h_uint32_t(), // Uncompressed size
        h_uint16_t(), // File name length
        h_uint16_t(), // Extra field length
        h_string_length(h_uint16_t()), // File name
        h_string_length(h_uint16_t()), // Extra field
        h_repeat(h_uint8_t(), h_uint32_t()), // File data
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
        return 1;
    }

    HStream *stream = h_stream_file_open(argv[1], "rb");
    if (!stream) {
        fprintf(stderr, "Failed to open file: %s\n", argv[1]);
        return 1;
    }

    HParser *parser = parse_zip();
    HParseResult *result = h_parse(parser, stream, NULL);
    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        h_stream_close(stream);
        return 1;
    }

    h_parse_result_free(result);
    h_stream_close(stream);
    return 0;
}