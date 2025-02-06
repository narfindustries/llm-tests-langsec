enum EI_CLASS : uint8 {
    ELFCLASSNONE = 0,
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
}

enum EI_DATA : uint8 {
    ELFDATANONE = 0,
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
}

enum EI_OSABI : uint8 {
    ELFOSABI_NONE = 0,
    ELFOSABI_SYSV = 0,
    ELFOSABI_HPUX = 1,
    ELFOSABI_NETBSD = 2,
    ELFOSABI_LINUX = 3,
    ELFOSABI_SOLARIS = 6,
    ELFOSABI_AIX = 7,
    ELFOSABI_IRIX = 8,
    ELFOSABI_FREEBSD = 9,
    ELFOSABI_TRU64 = 10,
    ELFOSABI_MODESTO = 11,
    ELFOSABI_OPENBSD = 12,
    ELFOSABI_ARM = 97,
    ELFOSABI_STANDALONE = 255
}

enum e_type : uint16 {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4,
    ET_LOOS = 0xFE00,
    ET_HIOS = 0xFEFF,
    ET_LOPROC = 0xFF00,
    ET_HIPROC = 0xFFFF
}

enum e_machine : uint16 {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_IAMCU = 6,
    EM_860 = 7,
    EM_MIPS = 8,
    EM_ARM = 40,
    EM_X86_64 = 62,
    EM_AARCH64 = 183,
    EM_RISCV = 243
}

enum sh_type : uint32 {
    SHT_NULL = 0,
    SHT_PROGBITS = 1,
    SHT_SYMTAB = 2,
    SHT_STRTAB = 3,
    SHT_RELA = 4,
    SHT_HASH = 5,
    SHT_DYNAMIC = 6,
    SHT_NOTE = 7,
    SHT_NOBITS = 8,
    SHT_REL = 9,
    SHT_SHLIB = 10,
    SHT_DYNSYM = 11,
    SHT_LOPROC = 0x70000000,
    SHT_HIPROC = 0x7FFFFFFF,
    SHT_LOUSER = 0x80000000,
    SHT_HIUSER = 0xFFFFFFFF
}

enum sh_flags : uint32 {
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4,
    SHF_MASKPROC = 0xF0000000
}

enum p_type : uint32 {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_TLS = 7,
    PT_LOOS = 0x60000000,
    PT_HIOS = 0x6FFFFFFF,
    PT_LOPROC = 0x70000000,
    PT_HIPROC = 0x7FFFFFFF
}

struct ElfHeader {
    uint8[4] e_ident_magic; // Should be 0x7f, 'E', 'L', 'F'
    EI_CLASS e_ident_class;
    EI_DATA e_ident_data;
    uint8 e_ident_version;
    EI_OSABI e_ident_osabi;
    uint8 e_ident_abiversion;
    uint8[7] e_ident_pad;
    e_type e_type;
    e_machine e_machine;
    uint32 e_version;
    uint32 e_entry;
    uint32 e_phoff;
    uint32 e_shoff;
    uint32 e_flags;
    uint16 e_ehsize;
    uint16 e_phentsize;
    uint16 e_phnum;
    uint16 e_shentsize;
    uint16 e_shnum;
    uint16 e_shstrndx;
}

struct ProgramHeader {
    p_type p_type;
    uint32 p_offset;
    uint32 p_vaddr;
    uint32 p_paddr;
    uint32 p_filesz;
    uint32 p_memsz;
    uint32 p_flags;
    uint32 p_align;
}

struct SectionHeader {
    uint32 sh_name;
    sh_type sh_type;
    sh_flags sh_flags;
    uint32 sh_addr;
    uint32 sh_offset;
    uint32 sh_size;
    uint32 sh_link;
    uint32 sh_info;
    uint32 sh_addralign;
    uint32 sh_entsize;
}

LittleEndian struct ELF {
    ElfHeader header;
    ProgramHeader[header.e_phnum] program_headers;
    SectionHeader[header.e_shnum] section_headers;
}