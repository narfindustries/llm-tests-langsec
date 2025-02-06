#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t version;
} TransactionHeader;

typedef struct {
    uint8_t txid[32];
    uint32_t vout;
    uint8_t scriptSigLen;
    uint8_t* scriptSig;
    uint32_t sequence;
} TransactionInput;

typedef struct {
    uint64_t value;
    uint8_t pkScriptLen;
    uint8_t* pkScript;
} TransactionOutput;

typedef struct {
    TransactionHeader header;
    size_t numInputs;
    TransactionInput* inputs;
    size_t numOutputs;
    TransactionOutput* outputs;
    uint32_t locktime;
} BitcoinTransaction;

static HParser* TransactionHeaderParser(void) {
    return h_uint32();
}

static HParser* TransactionInputParser(void) {
    return h_seq(
        h_bytes(32),
        h_uint32(),
        h_uint8(),
        h_bytes_n(sizeof(uint8_t)), //Fixed size for scriptSig
        h_uint32(),
        NULL
    );
}

static HParser* TransactionOutputParser(void) {
    return h_seq(
        h_uint64(),
        h_uint8(),
        h_bytes_n(sizeof(uint8_t)), //Fixed size for pkScript
        NULL
    );
}

static HParser* BitcoinTransactionParser(void) {
    return h_seq(
        TransactionHeaderParser(),
        h_array(TransactionInputParser()),
        h_array(TransactionOutputParser()),
        h_uint32(),
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HParseResult* result = h_parse(BitcoinTransactionParser(), buffer, fileSize);

    if (result->status == H_PARSE_OK) {
        BitcoinTransaction* tx = (BitcoinTransaction*)result->value;
        printf("Transaction parsed successfully:\n");
        printf("Version: %u\n", tx->header.version);
        printf("Number of Inputs: %zu\n", tx->numInputs);
        printf("Number of Outputs: %zu\n", tx->numOutputs);
        printf("Locktime: %u\n", tx->locktime);

        //Further processing of inputs and outputs would go here.  Note that
        //you'll need to handle the dynamically allocated scriptSig and pkScript
        //data appropriately.

        free(tx->inputs); //Free dynamically allocated memory
        free(tx->outputs);
        free(tx);
        h_free_result(result);

    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result->offset, result->error);
        h_free_result(result);
    }

    free(buffer);
    return 0;
}
