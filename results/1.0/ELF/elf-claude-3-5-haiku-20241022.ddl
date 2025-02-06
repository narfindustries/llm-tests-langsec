protocol elf {
    enum ElfClass : u8 {
        NONE = 0,
        CLASS32 = 1,
        CLASS64 = 2
    }

    enum ElfData : u8 {
        NONE = 0,
        LSB = 1,
        MSB = 2
    }

    enum ElfVersion : u8 {
        NONE = 0,
        CURRENT = 1
    }

    enum ElfOsAbi : u8 {
        SYSV = 0,
        HPUX = 1,
        LINUX = 3,
        FREEBSD = 9,
        SOLARIS = 6
    }

    enum ElfType : u16 {
        NONE = 0,
        REL = 1,
        EXEC = 2,
        DYN = 3,
        CORE = 4
    }

    enum ElfMachine : u16 {
        NONE = 0,
        M32 = 1,
        SPARC = 2,
        I386 = 3,
        M68K = 4,
        M88K = 5,
        X86_64 = 62
    }

    type ElfIdent = struct {
        magic: u32,
        class: ElfClass,
        data: ElfData,
        version: ElfVersion,
        os_abi: ElfOsAbi,
        abi_version: u8,
        padding: [u8; 7]
    }

    type ElfHeader32 = struct {
        ident: ElfIdent,
        type: ElfType,
        machine: ElfMachine,
        version: u32,
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

    type ElfHeader64 = struct {
        ident: ElfIdent,
        type: ElfType,
        machine: ElfMachine,
        version: u32,
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

    parser Elf32 = parse ElfHeader32

    parser Elf64 = parse ElfHeader64

    parser parse_elf = parse (
        match ident.class {
            ElfClass::CLASS32 => Elf32,
            ElfClass::CLASS64 => Elf64,
            _ => error
        }
    )
}