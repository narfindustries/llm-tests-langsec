#include <stdio.h>
#include <hammer/hammer.h> 

// Basic fixed-size integer types
static HParsedToken *act_uint16le(const HParseResult *p, void *user_data) {
    const uint8_t *bytes = h_seq_elements(p->ast, 0);
    return H_MAKE_UINT(((uint16_t)bytes[1] << 8) | bytes[0]);
}

static HParsedToken *act_uint32le(const HParseResult *p, void *user_data) {
    const uint8_t *bytes = h_seq_elements(p->ast, 0);
    return H_MAKE_UINT(((uint32_t)bytes[3] << 24) | ((uint32_t)bytes[2] << 16) | ((uint32_t)bytes[1] << 8) | bytes[0]);
}

static HParsedToken *act_uint64le(const HParseResult *p, void *user_data) {
    const uint8_t *bytes = h_seq_elements(p->ast, 0);
    return H_MAKE_UINT(((uint64_t)bytes[7] << 56) | ((uint64_t)bytes[6] << 48) | ((uint64_t)bytes[5] << 40) | ((uint64_t)bytes[4] << 32) | ((uint64_t)bytes[3] << 24) | ((uint64_t)bytes[2] << 16) | ((uint64_t)bytes[1] << 8) | bytes[0]);
}

// Varint parsing (compact representation for integers)
static HParsedToken *act_varint(const HParseResult *p, void *user_data) {
    uint64_t result = 0;
    int shift = 0;
    for (int i = 0; i < p->ast->seq->used; i++) {
        uint8_t byte = HPInt_get_uint(p->ast->seq->elements[i]->token);
        result |= (uint64_t)(byte & 0x7F) << shift;
        shift += 7;
        if ((byte & 0x80) == 0) break;
    }
    return H_MAKE_UINT(result);
}

static HParser *bitcoin_varint() {
    return h_action(h_length_value(h_uint8(), h_int_range(h_uint8(), 0x00, 0xFD)), act_varint, NULL);
}

// Parser for transaction input
static HParser *bitcoin_tx_in() {
    HParser *txid = h_repeat_n(h_uint8(), 32);
    HParser *vout = h_uint32le();
    HParser *script_len = bitcoin_varint();
    HParser *script_sig = h_action(h_length_value(script_len, h_uint8()), h_collect_uint8, NULL);
    HParser *seq = h_uint32le();

    return h_sequence(txid, vout, script_sig, seq, NULL);
}

// Parser for transaction output
static HParser *bitcoin_tx_out() {
    HParser *value = h_uint64le();
    HParser *script_len = bitcoin_varint();
    HParser *script_pubkey = h_action(h_length_value(script_len, h_uint8()), h_collect_uint8, NULL);

    return h_sequence(value, script_pubkey, NULL);
}

// Main parser for a Bitcoin transaction
static HParser *bitcoin_transaction() {
    HParser *version = h_uint32le();
    HParser *in_count = bitcoin_varint();
    HParser *inputs = h_many(bitcoin_tx_in(), in_count);
    HParser *out_count = bitcoin_varint();
    HParser *outputs = h_many(bitcoin_tx_out(), out_count);
    HParser *locktime = h_uint32le();

    return h_sequence(version, inputs, outputs, locktime, NULL);
}

int main(int argc, char **argv) {
    HParser *btc_parser = bitcoin_transaction();

    // parsing simulation
    const uint8_t testcase[] = {
        0x01, 0x00, 0x00, 0x00, 0x01, // Version
        // Inputs
        0x01, 0x78, 0x30, 0xF4, 0xC6, 0xAA, 0x42, 0xDA, 0x04, 0x43, 0x19,
        0x00, 0x00, 0x00, 0x00 // Locktime
    };
    size_t len = sizeof(testcase)/sizeof(testcase[0]);
    HParseResult *res = h_parse(btc_parser, testcase, len);
    if (res) {
        printf("Parsed successfully.\n");
    } else {
        printf("Failed to parse.\n");
    }

    h_parse_result_free(res);
    h_parser_free(btc_parser);
    return 0;
}