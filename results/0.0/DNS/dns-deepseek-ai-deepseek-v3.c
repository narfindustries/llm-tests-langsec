#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *dns_parser() {
    HParser *id = h_int16();
    HParser *qr = h_bits(1, false);
    HParser *opcode = h_bits(4, false);
    HParser *aa = h_bits(1, false);
    HParser *tc = h_bits(1, false);
    HParser *rd = h_bits(1, false);
    HParser *ra = h_bits(1, false);
    HParser *z = h_bits(3, false);
    HParser *rcode = h_bits(4, false);
    HParser *qdcount = h_int16();
    HParser *ancount = h_int16();
    HParser *nscount = h_int16();
    HParser *arcount = h_int16();

    HParser *header = h_sequence(id, qr, opcode, aa, tc, rd, ra, z, rcode, qdcount, ancount, nscount, arcount, NULL);

    HParser *label = h_sequence(h_int8(), h_length_value(h_int8(), h_uint8()), NULL);
    HParser *qname = h_many1(label);
    HParser *qtype = h_int16();
    HParser *qclass = h_int16();
    HParser *question = h_sequence(qname, qtype, qclass, NULL);

    HParser *questions = h_many(h_length_value(h_int16(), question));

    HParser *name = qname;
    HParser *type = h_int16();
    HParser *class = h_int16();
    HParser *ttl = h_int32();
    HParser *rdlength = h_int16();
    HParser *rdata = h_length_value(rdlength, h_uint8());
    HParser *resource_record = h_sequence(name, type, class, ttl, rdlength, rdata, NULL);

    HParser *answers = h_many(h_length_value(h_int16(), resource_record));
    HParser *authority = h_many(h_length_value(h_int16(), resource_record));
    HParser *additional = h_many(h_length_value(h_int16(), resource_record));

    return h_sequence(header, questions, answers, authority, additional, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = dns_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}