#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t magic[] = {0x7f, 'E', 'L', 'F'};

HParser* init_elf_parser(void) {
    // ELF Header fields
    HParser *h_magic = h_token(magic, sizeof(magic));
    HParser *h_class = h_uint8();
    HParser *h_data = h_uint8();
    HParser *h_version = h_uint8();
    HParser *h_osabi = h_uint8();
    HParser *h_abiversion = h_uint8();
    HParser *h_pad = h_repeat_n(h_uint8(), 7);
    HParser *h_type = h_uint16();
    HParser *h_machine = h_uint16();
    HParser *h_elfversion = h_uint32();
    HParser *h_entry = h_uint64();
    HParser *h_phoff = h_uint64();
    HParser *h_shoff = h_uint64();
    HParser *h_flags = h_uint32();
    HParser *h_ehsize = h_uint16();
    HParser *h_phentsize = h_uint16();
    HParser *h_phnum = h_uint16();
    HParser *h_shentsize = h_uint16();
    HParser *h_shnum = h_uint16();
    HParser *h_shstrndx = h_uint16();

    // Combine all fields in sequence
    return h_sequence(h_magic,
                     h_class,
                     h_data,
                     h_version,
                     h_osabi,
                     h_abiversion,
                     h_pad,
                     h_type,
                     h_machine,
                     h_elfversion,
                     h_entry,
                     h_phoff,
                     h_shoff,
                     h_flags,
                     h_ehsize,
                     h_phentsize,
                     h_phnum,
                     h_shentsize,
                     h_shnum,
                     h_shstrndx,
                     NULL);
}

int main(int argc, char *argv[]) {
    HParser *parser = init_elf_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}