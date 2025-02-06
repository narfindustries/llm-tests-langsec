#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser *elf_magic;
static HParser *elf_class;
static HParser *elf_data;
static HParser *elf_version;
static HParser *elf_osabi;
static HParser *elf_abiversion;
static HParser *elf_pad;
static HParser *elf_type;
static HParser *elf_machine;
static HParser *elf_version2;
static HParser *elf_entry;
static HParser *elf_phoff;
static HParser *elf_shoff;
static HParser *elf_flags;
static HParser *elf_ehsize;
static HParser *elf_phentsize;
static HParser *elf_phnum;
static HParser *elf_shentsize;
static HParser *elf_shnum;
static HParser *elf_shstrndx;
static HParser *elf_header;

static HParser *ph_type;
static HParser *ph_flags;
static HParser *ph_offset;
static HParser *ph_vaddr;
static HParser *ph_paddr;
static HParser *ph_filesz;
static HParser *ph_memsz;
static HParser *ph_align;
static HParser *program_header;

static HParser *sh_name;
static HParser *sh_type;
static HParser *sh_flags;
static HParser *sh_addr;
static HParser *sh_offset;
static HParser *sh_size;
static HParser *sh_link;
static HParser *sh_info;
static HParser *sh_addralign;
static HParser *sh_entsize;
static HParser *section_header;

void init_parsers(void) {
    elf_magic = h_token((uint8_t*)"\x7F""ELF", 4);
    elf_class = h_uint8();
    elf_data = h_uint8();
    elf_version = h_uint8();
    elf_osabi = h_uint8();
    elf_abiversion = h_uint8();
    elf_pad = h_repeat_n(h_uint8(), 7);
    elf_type = h_uint16();
    elf_machine = h_uint16();
    elf_version2 = h_uint32();
    elf_entry = h_uint64();
    elf_phoff = h_uint64();
    elf_shoff = h_uint64();
    elf_flags = h_uint32();
    elf_ehsize = h_uint16();
    elf_phentsize = h_uint16();
    elf_phnum = h_uint16();
    elf_shentsize = h_uint16();
    elf_shnum = h_uint16();
    elf_shstrndx = h_uint16();

    elf_header = h_sequence(
        elf_magic, elf_class, elf_data, elf_version,
        elf_osabi, elf_abiversion, elf_pad, elf_type,
        elf_machine, elf_version2, elf_entry, elf_phoff,
        elf_shoff, elf_flags, elf_ehsize, elf_phentsize,
        elf_phnum, elf_shentsize, elf_shnum, elf_shstrndx,
        NULL
    );

    ph_type = h_uint32();
    ph_flags = h_uint32();
    ph_offset = h_uint64();
    ph_vaddr = h_uint64();
    ph_paddr = h_uint64();
    ph_filesz = h_uint64();
    ph_memsz = h_uint64();
    ph_align = h_uint64();

    program_header = h_sequence(
        ph_type, ph_flags, ph_offset, ph_vaddr,
        ph_paddr, ph_filesz, ph_memsz, ph_align,
        NULL
    );

    sh_name = h_uint32();
    sh_type = h_uint32();
    sh_flags = h_uint64();
    sh_addr = h_uint64();
    sh_offset = h_uint64();
    sh_size = h_uint64();
    sh_link = h_uint32();
    sh_info = h_uint32();
    sh_addralign = h_uint64();
    sh_entsize = h_uint64();

    section_header = h_sequence(
        sh_name, sh_type, sh_flags, sh_addr,
        sh_offset, sh_size, sh_link, sh_info,
        sh_addralign, sh_entsize,
        NULL
    );
}

void print_parse_result(const HParsedToken* result, int depth) {
    for (int i = 0; i < depth; i++) printf("  ");
    
    if (result->token_type == TT_SEQUENCE) {
        printf("Sequence:\n");
        for (size_t i = 0; i < result->seq->used; i++) {
            print_parse_result(result->seq->elements[i], depth + 1);
        }
    } else if (result->token_type == TT_UINT) {
        printf("Value: %lu\n", result->uint);
    } else if (result->token_type == TT_BYTES) {
        printf("Bytes: ");
        for (size_t i = 0; i < result->bytes.len; i++) {
            printf("%02x ", result->bytes.token[i]);
        }
        printf("\n");
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    init_parsers();

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buf = malloc(size);
    if (fread(buf, 1, size, f) != size) {
        perror("Failed to read file");
        fclose(f);
        free(buf);
        return 1;
    }
    fclose(f);

    HParseResult *result = h_parse(elf_header, buf, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF header\n");
        free(buf);
        return 1;
    }

    printf("ELF Header:\n");
    print_parse_result(result->ast, 0);
    h_parse_result_free(result);

    result = h_parse(elf_phnum, buf + 56, 2);
    uint16_t phnum = result->ast->uint;
    h_parse_result_free(result);

    result = h_parse(elf_phoff, buf + 32, 8);
    uint64_t phoff = result->ast->uint;
    h_parse_result_free(result);

    for (uint16_t i = 0; i < phnum; i++) {
        result = h_parse(program_header, buf + phoff + i * 56, 56);
        if (!result) {
            fprintf(stderr, "Failed to parse program header %d\n", i);
            continue;
        }
        printf("\nProgram Header %d:\n", i);
        print_parse_result(result->ast, 0);
        h_parse_result_free(result);
    }

    result = h_parse(elf_shnum, buf + 60, 2);
    uint16_t shnum = result->ast->uint;
    h_parse_result_free(result);

    result = h_parse(elf_shoff, buf + 40, 8);
    uint64_t shoff = result->ast->uint;
    h_parse_result_free(result);

    for (uint16_t i = 0; i < shnum; i++) {
        result = h_parse(section_header, buf + shoff + i * 64, 64);
        if (!result) {
            fprintf(stderr, "Failed to parse section header %d\n", i);
            continue;
        }
        printf("\nSection Header %d:\n", i);
        print_parse_result(result->ast, 0);
        h_parse_result_free(result);
    }

    free(buf);
    return 0;
}