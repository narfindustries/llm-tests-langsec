struct ElfMagic {
    magic0: u8 where magic0 == 0x7F,
    magic1: u8 where magic1 == 'E',
    magic2: u8 where magic2 == 'L',
    magic3: u8 where magic3 == 'F'
}

enum ElfClass : u8 {
    ELFCLASSNONE = 0,
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
}

enum ElfDataEncoding : u8 {
    ELFDATANONE = 0,
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
}

enum ElfVersion : u8 {
    EV_NONE = 0,
    EV_CURRENT = 1
}

enum ElfOsAbi : u8 {
    ELFOSABI_SYSV = 0,
    ELFOSABI_LINUX = 3
}

enum ElfObjectType : u16 {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
}

enum ElfMachine : u16 {
    EM_NONE = 0,
    EM_386 = 3,
    EM_X86_64 = 62
}

struct ElfHeader32 {
    magic: ElfMagic,
    class: ElfClass,
    dataEncoding: ElfDataEncoding,
    version: ElfVersion,
    osAbi: ElfOsAbi,
    abiVersion: u8,
    padding: [u8; 7],
    type: ElfObjectType,
    machine: ElfMachine,
    elfVersion: ElfVersion,
    entry: u32,
    phoff: u32,
    shoff: u32,
    flags: u32,
    ehsize: u16,
    phentsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16
}

struct ElfHeader64 {
    magic: ElfMagic,
    class: ElfClass,
    dataEncoding: ElfDataEncoding,
    version: ElfVersion,
    osAbi: ElfOsAbi,
    abiVersion: u8,
    padding: [u8; 7],
    type: ElfObjectType,
    machine: ElfMachine,
    elfVersion: ElfVersion,
    entry: u64,
    phoff: u64,
    shoff: u64,
    flags: u32,
    ehsize: u16,
    phentsize: u16,
    phnum: u16,
    shentsize: u16,
    shnum: u16,
    shstrndx: u16
}

struct Elf {
    header: variant match (header.class) {
        ElfClass.ELFCLASS32 => ElfHeader32,
        ElfClass.ELFCLASS64 => ElfHeader64
    }
}