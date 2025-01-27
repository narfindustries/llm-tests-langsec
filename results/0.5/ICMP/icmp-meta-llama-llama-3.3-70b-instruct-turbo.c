#include <hammer/hammer.h>

HAMMER Specification {
    HAMMER Input {
        struct icmp_header {
            uint8_t type;
            uint8_t code;
            uint16_t checksum;
            uint32_t id;
            uint16_t seq;
        };
    }

    HAMMER Output {
        struct icmp_header output;
    }

    HAMMER Transform {
        output.type = input->type;
        output.code = input->code;
        output.checksum = 0;
        output.id = input->id;
        output.seq = input->seq;
    }

    HAMMER Emit {
        output.checksum = calculate_checksum(&output);
    }
}

uint16_t calculate_checksum(struct icmp_header *packet) {
    uint32_t sum = 0;
    uint16_t *ptr = (uint16_t *) packet;
    int len = sizeof(struct icmp_header);

    while (len > 1) {
        sum += *ptr;
        ptr++;
        len -= 2;
    }

    if (len == 1) {
        sum += *(uint8_t *) ptr;
    }

    sum = (sum >> 16) + (sum & 0xffff);
    sum += (sum >> 16);

    return ~sum;
}