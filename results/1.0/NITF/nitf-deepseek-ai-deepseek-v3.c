#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for NITF fields
HParser *nitf_file_header_parser() {
    return h_sequence(
        h_token("NITF", 4), // FHDR
        h_length_value(h_uint32(), h_uint8()), // FTITLE
        h_choice( // FSCLAS
            h_token("U", 1),
            h_token("C", 1),
            h_token("S", 1),
            NULL
        ),
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSCODE
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSCTLH
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSREL
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSDCTP
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSDCDT
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSDCXM
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSDG
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSDGDT
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSCLTX
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSCOP
        h_optional(h_length_value(h_uint32(), h_uint8())), // FSCPYS
        h_optional(h_length_value(h极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极极極