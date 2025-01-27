#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations
static HParsedToken *act_NTPHeader(const HParseResult *p, void *user_data);
static HParsedToken *act_NTPExtension(const HParseResult *p, void *user_data);

// Bits and pieces
static HParser *bit = h_bits(1, false);
static HParser *octet = h_bits(8, false);
static HParser *word = h_bits(32, false);

// NTP Packet structure
static HParser *leap_indicator = h_bits(2, false);
static HParser *version_number = h_bits(3, false);
static HParser *mode = h_bits(3, false);
static HParser *stratum = octet;
static HParser *poll = octet;
static HParser *precision = octet;
static HParser *root_delay = word;
static HParser *root_dispersion = word;
static HParser *reference_identifier = word;
static HParser *reference_timestamp = h_sequence(word, word, NULL);
static HParser *originate_timestamp = h_sequence(word, word, NULL);
static HParser *receive_timestamp = h_sequence(word, word, NULL);
static HParser *transmit_timestamp = h_sequence(word, word, NULL);
static HParser *extension_bytes = h_many(h_int_range(octet, 0, 255));
static HParser *authenticator = h_optional(h_sequence(octet, octet, octet, octet, NULL));

// NTP Header sequence
static HParser *ntp_header = h_action(h_sequence(
    leap_indicator,
    version_number,
    mode,
    stratum,
    poll,
    precision,
    root_delay,
    root_dispersion,
    reference_identifier,
    reference_timestamp,
    originate_timestamp,
    receive_timestamp,
    transmit_timestamp,
    extension_bytes,
    authenticator,
    NULL), act_NTPHeader, NULL);

// NTP Extension (optional)
static HParser *ntp_extension = h_action(h_sequence(
    word,  // Field type
    word,  // Length
    h_many(octet, NULL),  // Value
    NULL), act_NTPExtension, NULL);

// Full NTP Packet
static HParser *ntp_packet = h_sequence(
    ntp_header,
    h_many(ntp_extension, NULL),
    NULL);

// Actions
static HParsedToken *act_NTPHeader(const HParseResult *p, void *user_data) {
    // Process NTP header
    return NULL;
}

static HParsedToken *act_NTPExtension(const HParseResult *p, void *user_data) {
    // Process NTP extensions
    return NULL;
}

int main(int argc, char *argv[]) {
    HParser *parser = ntp_packet;
    HAllocator *mm__ = h_system_allocator;
    HParseResult *result = h_parse(parser, (const uint8_t *)"\xe3\x00\x06\xec\xfa\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x06", 48, mm__);
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed.\n");
    }
    h_parse_result_free(result);
    return 0;
}
This code sample attempts to parse a basic NTP packet using the Hammer parsing library. The packet structure includes fields for version number, mode, timestamps, and optional extension fields. The use of `h_action` is to potentially process fields post-parsing, although the actions are placeholders returning `NULL`. Adjustments may be needed for specific requirements or error handling in a real-world application.