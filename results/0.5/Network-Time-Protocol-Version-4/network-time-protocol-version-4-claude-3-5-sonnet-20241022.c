#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t VERSION_MASK = 0x38;
static const uint8_t MODE_MASK = 0x07;

HParser* init_ntp_parser() {
    // Individual field parsers
    HParser* li = h_bits(2, false);
    HParser* vn = h_bits(3, false);
    HParser* mode = h_bits(3, false);
    
    HParser* stratum = h_uint8();
    HParser* poll = h_uint8();
    HParser* precision = h_int8();
    
    HParser* root_delay = h_uint32();
    HParser* root_dispersion = h_uint32();
    HParser* reference_id = h_uint32();
    
    HParser* reference_timestamp = h_uint64();
    HParser* originate_timestamp = h_uint64();
    HParser* receive_timestamp = h_uint64();
    HParser* transmit_timestamp = h_uint64();

    // Optional extension fields and MAC
    HParser* extension_fields = h_many(h_uint32());
    HParser* mac = h_many(h_uint8());

    // Combine all fields in sequence
    return h_sequence(li, vn, mode,
                     stratum, poll, precision,
                     root_delay, root_dispersion, reference_id,
                     reference_timestamp, originate_timestamp,
                     receive_timestamp, transmit_timestamp,
                     extension_fields, mac,
                     NULL);
}

int main(int argc, char** argv) {
    HParser* parser = init_ntp_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}