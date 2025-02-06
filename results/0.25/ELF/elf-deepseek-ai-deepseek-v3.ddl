Elf {
    e_ident: array[16] of uint8 {
        EI_MAG0: uint8 = 0x7F;
        EI_MAG1: uint8 = 'E';
        EI_MAG2: uint8 = 'L';
        EI_MAG3: uint8 = 'F';
        EI_CLASS: uint8 {
            ELFCLASSNONE: 0;
            ELFCLASS32: 1;
            ELFCLASS64: 2;
        };
        EI_DATA: uint8 {
            ELFDATANONE: 0;
            ELFDATA2LSB: 1;
            ELFDATA2MSB: 2;
        };
        EI_VERSION: uint8 {
            EV_NONE: 0;
            EV_CURRENT: 1;
        };
        EI_OSABI: uint8 {
            ELFOSABI_NONE: 0;
            ELFOSABI_LINUX: 3;
            ELFOSABI_SOLARIS: 6;
        };
        EI_ABIVERSION: uint8;
        EI_PAD: array[7] of uint8;
    };
    e_type: uint16 {
        ET_NONE: 0;
        ET_REL: 1;
        ET_EXEC: 2;
        ET_DYN: 3;
        ET_CORE: 4;
    };
    e_machine: uint16 {
        EM_NONE: 0;
        EM_X86_64: 62;
        EM_ARM: 40;
        EM_AARCH64: 183;
    };
    e_version: uint32 {
        EV_NONE: 0;
        EV_CURRENT: 1;
    };
    e_entry: uint32 | uint64;
    e_phoff: uint32 | uint64;
    e_shoff: uint32 | uint64;
    e_flags: uint32;
    e_ehsize: uint16;
    e_phentsize: uint16;
    e_phnum: uint16;
    e_shentsize: uint16;
    e_shnum: uint16;
    e_shstrndx: uint16;
    program_headers: array[e_phnum] of ProgramHeader;
    section_headers: array[e_shnum] of SectionHeader;
}

ProgramHeader {
    p_type: uint32 {
        PT_NULL: 0;
        PT_LOAD: 1;
        PT_DYNAMIC: 2;
        PT_INTERP: 3;
    };
    p_flags: uint32 {
        PF_X: 1;
        PF_W: 2;
        PF_R: 4;
    };
    p_offset: uint32 | uint64;
    p_vaddr: uint32 | uint64;
    p_paddr: uint32 | uint64;
    p_filesz: uint32 | uint64;
    p_memsz: uint32 | uint64;
    p_align: uint32 | uint64;
}

SectionHeader {
    sh_name: uint32;
    sh_type: uint32 {
        SHT_NULL: 0;
        SHT_PROGBITS: 1;
        SHT_SYMTAB: 2;
        SHT_STRTAB: 3;
    };
    sh_flags: uint32 | uint64 {
        SHF_WRITE: 1;
        SHF_ALLOC: 2;
        SHF_EXECINSTR: 4;
    };
    sh_addr: uint32 | uint64;
    sh_offset: uint32 | uint64;
    sh_size: uint32 | uint64;
    sh_link: uint32;
    sh_info: uint32;
    sh_addralign: uint32 | uint64;
    sh_entsize: uint32 | uint64;
}

SymbolTable {
    st_name: uint32;
    st_value: uint32 | uint64;
    st_size: uint32 | uint64;
    st_info: uint8 {
        STB_LOCAL: 0;
        STB_GLOBAL: 1;
        STB_WEAK: 2;
        STT_NOTYPE: 0;
        STT_OBJECT: 1;
        STT_FUNC: 2;
        STT_SECTION: 3;
        STT_FILE: 4;
    };
    st_other: uint8 {
        STV_DEFAULT: 0;
        STV_INTERNAL: 1;
        STV_HIDDEN: 2;
        STV_PROTECTED: 3;
    };
    st_shndx: uint16;
}