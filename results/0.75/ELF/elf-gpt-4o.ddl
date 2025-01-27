import "base" {
    from base import *;
}

struct ELF {
    magic: MagicBytes;
    class: ElfClass;
    data: ElfData;
    version: u8;
    osabi: u8;
    abiversion: u8;
    pad: PadBytes;
    type: ElfType;
    machine: ElfMachine;
    entry: u32;
    phoff: u32;
    shoff: u32;
    flags: u32;
    ehsize: u16;
    phentsize: u16;
    phnum: u16;
    shentsize: u16;
    shnum: u16;
    shstrndx: u16;
    ph: Array<ProgramHeader>[phnum];
    sh: Array<SectionHeader>[shnum];
}

struct MagicBytes {
    bytes: u8[4];
    assert bytes == [0x7F, 'E', 'L', 'F'];
}

enum ElfClass : u8 {
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
}

enum ElfData : u8 {
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
}

enum ElfType : u16 {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
}

enum ElfMachine : u16 {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_860 = 7,
    EM_MIPS = 8
}

struct PadBytes {
    bytes: u8[7];
}

struct ProgramHeader {
    type: u32;
    offset: u32;
    vaddr: u32;
    paddr: u32;
    filesz: u32;
    memsz: u32;
    flags: u32;
    align: u32;
}

struct SectionHeader {
    name: u32;
    type: u32;
    flags: u32;
    addr: u32;
    offset: u32;
    size: u32;
    link: u32;
    info: u32;
    addralign: u32;
    entsize: u32;
}