enum u8 ElfClass {
    ELFCLASSNONE = 0,
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
}

enum u8 ElfData {
    ELFDATANONE = 0,
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
}

enum u8 ElfVersion {
    EV_NONE = 0,
    EV_CURRENT = 1
}

enum u8 ElfOSABI {
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
    ELFOSABI_OPENVMS = 13,
    ELFOSABI_NSK = 14,
    ELFOSABI_AROS = 15,
    ELFOSABI_FENIXOS = 16,
    ELFOSABI_CLOUDABI = 17,
    ELFOSABI_OPENVOS = 18
}

enum u16 ElfType {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
}

enum u16 ElfMachine {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_860 = 7,
    EM_MIPS = 8,
    EM_PARISC = 15,
    EM_SPARC32PLUS = 18,
    EM_PPC = 20,
    EM_PPC64 = 21,
    EM_S390 = 22,
    EM_ARM = 40,
    EM_SH = 42,
    EM_SPARCV9 = 43,
    EM_IA_64 = 50,
    EM_X86_64 = 62,
    EM_VAX = 75
}

enum u32 SegmentType {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_TLS = 7
}

enum u32 SectionType {
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
}

enum u32 SectionFlags {
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4
}

enum u32 SegmentFlags {
    PF_X = 0x1,
    PF_W = 0x2,
    PF_R = 0x4
}

struct ElfHeader {
    u32 magic;
    ElfClass class;
    ElfData data;
    ElfVersion version;
    ElfOSABI osabi;
    u8 abiversion;
    u8 pad[7];
    ElfType type;
    ElfMachine machine;
    u32 version2;
    u64 entry;
    u64 phoff;
    u64 shoff;
    u32 flags;
    u16 ehsize;
    u16 phentsize;
    u16 phnum;
    u16 shentsize;
    u16 shnum;
    u16 shstrndx;
}

struct ProgramHeader {
    SegmentType type;
    SegmentFlags flags;
    u64 offset;
    u64 vaddr;
    u64 paddr;
    u64 filesz;
    u64 memsz;
    u64 align;
}

struct SectionHeader {
    u32 name;
    SectionType type;
    SectionFlags flags;
    u64 addr;
    u64 offset;
    u64 size;
    u32 link;
    u32 info;
    u64 addralign;
    u64 entsize;
}

struct ElfFile {
    ElfHeader header;
    ProgramHeader programHeaders[header.phnum];
    SectionHeader sectionHeaders[header.shnum];
}