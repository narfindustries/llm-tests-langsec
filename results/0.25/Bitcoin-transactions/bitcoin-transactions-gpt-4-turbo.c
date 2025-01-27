#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Basic parsers for Bitcoin transaction components
static HParser *u8 = h_uint8();
static HParser *u32 = h_uint32();
static HParser *u64 = h_uint64();
static HParser *var_int;
static HParser *var_str;
static HParser *tx_in;
static HParser *tx_out;
static HParser *tx;

// Parser for variable-length integers
static HParsedToken *act_parse_var_int(const HParseResult *p, void *user_data) {
    uint64_t value = 0;
    size_t len = p->ast->seq->used;
    for (size_t i = 0; i < len; i++) {
        uint8_t byte = H_CAST_UINT(p->ast->seq->elements[i]);
        value |= (uint64_t)byte << (i * 8);
    }
    return H_MAKE_UINT(value);
}

static HParser *build_var_int() {
    HParser *var_int_16 = h_sequence(h_bits(8, false), h_bits(8, false), NULL);
    HParser *var_int_32 = h_sequence(h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), NULL);
    HParser *var_int_64 = h_sequence(h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false),
                                     h_bits(8, false), h_bits(8, false), h_bits(8, false), h_bits(8, false), NULL);

    return h_choice(h_bits(8, false),
                    h_sequence(h_ch(0xFD), var_int_16, NULL),
                    h_sequence(h_ch(0xFE), var_int_32, NULL),
                    h_sequence(h_ch(0xFF), var_int_64, NULL),
                    NULL);
}

// Parser for variable-length strings
static HParsedToken *act_parse_var_str(const HParseResult *p, void *user_data) {
    const HCountedArray *arr = p->ast->seq->elements[1]->seq;
    return H_MAKE_BYTES(arr->bytes, arr->used);
}

static HParser *build_var_str() {
    return h_action(h_sequence(var_int, h_bytes(0), NULL), act_parse_var_str, NULL);
}

// Parser for a transaction input
static HParsedToken *act_parse_tx_in(const HParseResult *p, void *user_data) {
    // Custom action can be added here
    return NULL;
}

static HParser *build_tx_in() {
    return h_action(h_sequence(h_bytes(32), u32, var_str, u32, NULL), act_parse_tx_in, NULL);
}

// Parser for a transaction output
static HParsedToken *act_parse_tx_out(const HParseResult *p, void *user_data) {
    // Custom action can be added here
    return NULL;
}

static HParser *build_tx_out() {
    return h_action(h_sequence(u64, var_str, NULL), act_parse_tx_out, NULL);
}

// Parser for a Bitcoin transaction
static HParsedToken *act_parse_tx(const HParseResult *p, void *user_data) {
    // Custom action can be added here
    return NULL;
}

static HParser *build_tx() {
    return h_action(h_sequence(u32, h_many(tx_in), h_many(tx_out), u32, NULL), act_parse_tx, NULL);
}

int main(int argc, char **argv) {
    // Initialize parsers
    var_int = build_var_int();
    var_str = build_var_str();
    tx_in = build_tx_in();
    tx_out = build_tx_out();
    tx = build_tx();

    // Example usage: parse a binary stream of a Bitcoin transaction
    uint8_t data[] = { /* Example binary data of a Bitcoin transaction */ };
    size_t length = sizeof(data);
    HParseResult *result = h_parse(tx, data, length);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }

    // Clean up
    h_parse_result_free(result);
    h_parser_free(tx);
    h_parser_free(tx_out);
    h_parser_free(tx_in);
    h_parser_free(var_str);
    h_parser_free(var_int);

    return 0;
}