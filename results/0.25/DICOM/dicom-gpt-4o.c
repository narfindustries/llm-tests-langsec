#include <hammer/hammer.h>

HParser *dicom_parser() {
    // Define basic parsers
    HParser *preamble = h_repeat_n(h_uint8(), 128);
    HParser *prefix = h_sequence(h_ch('D'), h_ch('I'), h_ch('C'), h_ch('M'), NULL);

    // Element parsers
    HParser *group_number = h_uint16();
    HParser *element_number = h_uint16();
    HParser *vr = h_choice(
        h_sequence(h_ch('O'), h_ch('B'), NULL),
        h_sequence(h_ch('O'), h_ch('W'), NULL),
        h_sequence(h_ch('U'), h_ch('N'), NULL),
        h_sequence(h_ch('S'), h_ch('Q'), NULL),
        h_sequence(h_ch('A'), h_ch('E'), NULL),
        h_sequence(h_ch('A'), h_ch('S'), NULL),
        h_sequence(h_ch('A'), h_ch('T'), NULL),
        h_sequence(h_ch('D'), h_ch('A'), NULL),
        h_sequence(h_ch('D'), h_ch('S'), NULL),
        h_sequence(h_ch('D'), h_ch('T'), NULL),
        h_sequence(h_ch('F'), h_ch('L'), NULL),
        h_sequence(h_ch('F'), h_ch('D'), NULL),
        h_sequence(h_ch('I'), h_ch('S'), NULL),
        h_sequence(h_ch('L'), h_ch('O'), NULL),
        h_sequence(h_ch('L'), h_ch('T'), NULL),
        h_sequence(h_ch('P'), h_ch('N'), NULL),
        h_sequence(h_ch('S'), h_ch('H'), NULL),
        h_sequence(h_ch('S'), h_ch('L'), NULL),
        h_sequence(h_ch('S'), h_ch('S'), NULL),
        h_sequence(h_ch('S'), h_ch('T'), NULL),
        h_sequence(h_ch('T'), h_ch('M'), NULL),
        h_sequence(h_ch('U'), h_ch('I'), NULL),
        h_sequence(h_ch('U'), h_ch('L'), NULL),
        h_sequence(h_ch('U'), h_ch('S'), NULL),
        h_sequence(h_ch('U'), h_ch('T'), NULL),
        NULL
    );

    HParser *reserved = h_uint16();
    HParser *length = h_uint32();
    HParser *value = h_length_value(length, h_uint8());

    HParser *element = h_sequence(
        group_number,
        element_number,
        vr,
        reserved,
        length,
        value,
        NULL
    );

    // Define the DICOM parser
    HParser *dicom_file = h_sequence(
        preamble,
        prefix,
        h_many(element),
        NULL
    );

    return dicom_file;
}