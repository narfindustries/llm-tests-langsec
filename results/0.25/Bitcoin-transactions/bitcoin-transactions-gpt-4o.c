#include <hammer/hammer.h>

// Define the Bitcoin transaction structure
HParser *var_int = h_choice(
    h_sequence(h_uint8(), h_assert(h_peek_uint8() < 0xfd), NULL),
    h_sequence(h_uint8(), h_uint16(), h_assert(h_peek_uint8() == 0xfd), NULL),
    h_sequence(h_uint8(), h_uint32(), h_assert(h_peek_uint8() == 0xfe), NULL),
    h_sequence(h_uint8(), h_uint64(), h_assert(h_peek_uint8() == 0xff), NULL),
    NULL
);

HParser *hash = h_repeat_n(h_uint8(), 32);

HParser *tx_in = h_sequence(
    hash,               // Previous transaction hash
    h_uint32(),         // Output index
    var_int,            // Script length
    h_data(h_last_uint()), // Script
    h_uint32(),         // Sequence
    NULL
);

HParser *tx_out = h_sequence(
    h_uint64(),         // Value
    var_int,            // Script length
    h_data(h_last_uint()), // Script
    NULL
);

HParser *tx = h_sequence(
    h_uint32(),         // Version
    var_int,            // Input count
    h_repeat(tx_in, h_last_uint()), // Inputs
    var_int,            // Output count
    h_repeat(tx_out, h_last_uint()), // Outputs
    h_uint32(),         // Lock time
    NULL
);

HParser *bitcoin_transaction = h_sequence(
    tx,                 // Transaction
    NULL
);