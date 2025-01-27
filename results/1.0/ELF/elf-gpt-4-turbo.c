#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic data types
static HParser *int8 = h_int8();
static HParser *uint8 = h_uint8();
static HParser *int16 = h_int16();
static HParser *uint16 = h_uint16();
static HParser *int32 = h_int32();
static HParser *uint32 = h_uint32();
static HParser *int64 = h_int64();
static HParser *uint64 = h_uint64();

// ELF header
static HParser *elf_magic = h_sequence(h_ch_range(0x7F, 0x7F), h_ch('E'), h_ch('L'), h_ch('F'), NULL);
static HParser *elf_class = h_choice(h_int_range(h_uint8(), 1, 2), NULL); // 1=32-bit, 2=64-bit
static HParser *elf_data = h_choice(h_int_range(h_uint8(), 1, 2), NULL);  // 1=little endian, 2=big endian
static HParser *elf_version = h_int_range(h_uint8(), 1, 1);
static HParser *elf_osabi = h_uint8();
static HParser *elf_abiversion = h_uint8();
static HParser *elf_pad = h_repeat_n(h_uint8(), 7);
static HParser *elf_type = h_uint16();
static HParser *elf_machine = h_uint16();
static HParser *elf_eversion = h_uint32();
static HParser *elf_entry = h_uint64(); // Assumes 64-bit for simplicity, adjust per actual class
static HParser *elf_phoff = h_uint64(); // Program header offset
static HParser *elf_shoff = h_uint64(); // Section header offset
static HParser *elf_flags = h_uint32();
static HParser *elf_ehsize = h_uint16();
static HParser *elf_phentsize = h_uint16();
static HParser *elf_phnum = h_uint16();
static HParser *elf_shentsize = h_uint16();
static HParser *elf_shnum = h_uint16();
static HParser *elf_shstrndx = h_uint16();

HParser *elf_header = h_sequence(
    elf_magic, elf_class, elf_data, elf_version,
    elf_osabi, elf_abiversion, elf_pad,
    elf_type, elf_machine, elf_eversion, elf_entry,
    elf_phoff, elf_shoff, elf_flags, elf_ehsize,
    elf_phentsize, elf_phnum, elf_shentsize,
    elf_shnum, elf_shstrndx,
    NULL
);

int main(int argc, char **argv) {
    HParser *parser = elf_header;
    HParseResult *result = h_parse(parser, input_buffer, input_length);
    if (result) {
        // Successfully parsed the ELF header
        printf("ELF Header Parsed Successfully.\n");
    } else {
        // Error during parsing
        fprintf(stderr, "Failed to parse ELF Header.\n");
    }
    return 0;
}