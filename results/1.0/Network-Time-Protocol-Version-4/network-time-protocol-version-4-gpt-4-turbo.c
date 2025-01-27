#include <hammer/hammer.h>
#include <hammer/glue.h>

// Helper parsers for various data types
static HParser *b8;
static HParser *b16le;
static HParser *b16be;
static HParser *b32le;
static HParser *b32be;
static HParser *b64le;
static HParser *b64be;

// NTP short format
typedef struct {
    uint16_t seconds;
    uint16_t fraction;
} ntp_short_t;

static HParsedToken *act_pack_ntp_short(const HParseResult *p, void *user_data) {
    const HVArray *array = (const HVArray*)p->ast;
    ntp_short_t res;
    res.seconds = H_CAST_UINT(array->elements[0]);
    res.fraction = H_CAST_UINT(array->elements[1]);
    return H_MAKE_UINT(res.seconds << 16 | res.fraction);
}

static HParser *ntp_short() {
    return h_action(h_sequence(b16be, b16be, NULL),
                    act_pack_ntp_short, NULL);
}


// NTP full message format
typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint32_t ref_ts_sec;
    uint32_t ref_ts_frac;
    uint32_t orig_ts_sec;
    uint32_t orig_ts_frac;
    uint32_t recv_ts_sec;
    uint32_t recv_ts_frac;
    uint32_t trans_ts_sec;
    uint32_t trans_ts_frac;
} ntp_packet_t;

static HParsedToken *act_pack_ntp_packet(const HParseResult *p, void *user_data) {
    HVArray *array = h_seq_elements(p->ast);
    ntp_packet_t packet;

    packet.li_vn_mode = H_CAST_UINT8(array->elements[0]);
    packet.stratum = H_CAST_UINT8(array->elements[1]);
    packet.poll = H_CAST_UINT8(array->elements[2]);
    packet.precision = H_CAST_UINT8(array->elements[3]);
    packet.root_delay = H_CAST_UINT(array->elements[4]);
    packet.root_dispersion = H_CAST_UINT(array->elements[5]);
    packet.ref_id = H_CAST_UINT(array->elements[6]);
    packet.ref_ts_sec = H_CAST_UINT(array->elements[7]);
    packet.ref_ts_frac = H_CAST_UINT(array->elements[8]);
    packet.orig_ts_sec = H_CAST_UINT(array->elements[9]);
    packet.orig_ts_frac = H_CAST_UINT(array->elements[10]);
    packet.recv_ts_sec = H_CAST_UINT(array->elements[11]);
    packet.recv_ts_frac = H_CAST_UINT(array->elements[12]);
    packet.trans_ts_sec = H_CAST_UINT(array->elements[13]);
    packet.trans_ts_frac = H_CAST_UINT(array->elements[14]);

    return H_MAKE_BYTES(&packet, sizeof(packet));
}

static HParser *ntp_packet() {
    return h_action(h_sequence(b8, b8, b8, b8,
                               b32be, b32be, b32be,
                               b32be, b32be,
                               b32be, b32be,
                               b32be, b32be,
                               b32be, b32be, NULL),
                    act_pack_ntp_packet, NULL);
}

void init_parsers() {
    b8 = h_uint8();
    b16le = h_uint16_le();
    b16be = h_uint16_be();
    b32le = h_uint32_le();
    b32be = h_uint32_be();
    b64le = h_uint64_le();
    b64be = h_uint64_be();
}

int main(int argc, char *argv[]) {
    init_parsers();
    HParser *ntp_proto_parser = ntp_packet();
    
    // Suppose we now have a way to get binary data, placeholder here
    const uint8_t binary_data[] = {
        // A valid NTP packet binary data as a placeholder
    };
    size_t binary_data_size = sizeof(binary_data);

    HParseResult *result = h_parse(ntp_proto_parser, binary_data, binary_data_size);
    if (result) {
        printf("NTP packet parsed successfully\n");
        // Function to print packet fields or inspect further goes here
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse NTP packet\n");
    }

    h_free_parser(ntp_proto_parser);
    return 0;
}