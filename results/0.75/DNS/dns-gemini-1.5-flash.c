#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t id;
    uint8_t  qr:1;
    uint8_t  opcode:4;
    uint8_t  aa:1;
    uint8_t  tc:1;
    uint8_t  rd:1;
    uint8_t  ra:1;
    uint8_t  z:3;
    uint8_t  rcode:4;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header;

typedef struct {
    uint8_t *name;
    uint16_t type;
    uint16_t klass;
    uint32_t ttl;
    uint16_t rdlength;
    uint8_t *rdata;
} dns_resource_record;

HParser dns_domain_name(HParser p) {
    HParser label = HMap(HSeq(HBytes(1), HTake(HBytes(1))),
                         [](HResult r){
                             uint8_t len = r.results[0].value.bytes[0];
                             return HOk(r.results[1].value.bytes);
                         });
    return HMany1(label, HEnd);
}


HParser dns_header_parser() {
    return HMap(HSeq(HUInt16, HUInt16, HUInt16, HUInt16, HUInt8, HUInt8, HUInt16, HUInt16, HUInt16),
                 [](HResult r){
                     dns_header *h = malloc(sizeof(dns_header));
                     h->id = r.results[0].value.u;
                     h->qr = (r.results[4].value.u >> 7) & 1;
                     h->opcode = (r.results[4].value.u >> 3) & 0xF;
                     h->aa = (r.results[4].value.u >> 2) & 1;
                     h->tc = (r.results[4].value.u >> 1) & 1;
                     h->rd = r.results[4].value.u & 1;
                     h->ra = (r.results[5].value.u >> 7) & 1;
                     h->z = (r.results[5].value.u >> 4) & 0x7;
                     h->rcode = r.results[5].value.u & 0xF;
                     h->qdcount = r.results[6].value.u;
                     h->ancount = r.results[7].value.u;
                     h->nscount = r.results[8].value.u;
                     return HOk(h);
                 });
}


HParser dns_resource_record_parser() {
    return HMap(HSeq(dns_domain_name, HUInt16, HUInt16, HUInt32, HUInt16, HTake(HUInt16)),
                [](HResult r){
                    dns_resource_record *rr = malloc(sizeof(dns_resource_record));
                    rr->name = (uint8_t*)r.results[0].value.p;
                    rr->type = r.results[1].value.u;
                    rr->klass = r.results[2].value.u;
                    rr->ttl = r.results[3].value.u;
                    rr->rdlength = r.results[4].value.u;
                    rr->rdata = (uint8_t*)r.results[5].value.p;
                    return HOk(rr);
                });
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fileSize);
    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HParser dns_parser = HSeq(dns_header_parser, HMany(dns_resource_record_parser));

    HResult result = HParse(dns_parser, buffer, fileSize);

    if (result.status == H_OK) {
        printf("DNS message parsed successfully!\n");
    } else {
        fprintf(stderr, "Error parsing DNS message: %s\n", HErrorMessage(result.status));
    }

    free(buffer);
    return 0;
}
