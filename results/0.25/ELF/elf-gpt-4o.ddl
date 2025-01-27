module ELF;

type ElfHeader = struct {
    magic: Magic;
    class: ElfClass;
    data: ElfData;
    version: u8;
    os_abi: u8;
    abi_version: u8;
    pad: u64;
    type: ElfType;
    machine: ElfMachine;
    version2: u32;
    entry: u64;
    phoff: u64;
    shoff: u64;
    flags: u32;
    ehsize: u16;
    phentsize: u16;
    phnum: u16;
    shentsize: u16;
    shnum: u16;
    shstrndx: u16;
};

type Magic = struct {
    magic: u32;
} where {
    magic == 0x7f454c46; // "\x7FELF"
};

enum ElfClass : u8 {
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
};

enum ElfData : u8 {
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
};

enum ElfType : u16 {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
};

enum ElfMachine : u16 {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_860 = 7,
    EM_MIPS = 8
};

type ProgramHeader = struct {
    type: SegmentType;
    flags: u32;
    offset: u64;
    vaddr: u64;
    paddr: u64;
    filesz: u64;
    memsz: u64;
    align: u64;
};

enum SegmentType : u32 {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_TLS = 7
};

type SectionHeader = struct {
    name: u32;
    type: SectionType;
    flags: u64;
    addr: u64;
    offset: u64;
    size: u64;
    link: u32;
    info: u32;
    addralign: u64;
    entsize: u64;
};

enum SectionType : u32 {
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
    SHT_DYNSYM = 11
};

type ElfFile = struct {
    header: ElfHeader;
    program_headers: [ProgramHeader] (header.phnum);
    section_headers: [SectionHeader] (header.shnum);
};