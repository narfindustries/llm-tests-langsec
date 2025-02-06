elf {
    e_ident: array[16] of byte {
        EI_MAG0: byte = 0x7F;
        EI_MAG1: byte = 'E';
        EI_MAG2: byte = 'L';
        EI_MAG3: byte = 'F';
        EI_CLASS: byte {
            ELFCLASSNONE = 0;
            ELFCLASS32 = 1;
            ELFCLASS64 = 2;
        };
        EI_DATA: byte {
            ELFDATANONE = 0;
            ELFDATA2LSB = 1;
            ELFDATA2MSB = 2;
        };
        EI_VERSION: byte {
            EV_NONE = 0;
            EV_CURRENT = 1;
        };
        EI_OSABI: byte {
            ELFOSABI_NONE = 0;
            ELFOSABI_SYSV = 0;
            ELFOSABI_HPUX = 1;
            ELFOSABI_NETBSD = 2;
            ELFOSABI_LINUX = 3;
            ELFOSABI_SOLARIS = 6;
            ELFOSABI_AIX = 7;
            ELFOSABI_IRIX = 8;
            ELFOSABI_FREEBSD = 9;
            ELFOSABI_TRU64 = 10;
            ELFOSABI_MODESTO = 11;
            ELFOSABI_OPENBSD = 12;
            ELFOSABI_ARM_AEABI = 64;
            ELFOSABI_ARM = 97;
            ELFOSABI_STANDALONE = 255;
        };
        EI_ABIVERSION: byte;
        EI_PAD: array[7] of byte;
    };
    e_type: uint16 {
        ET_NONE = 0;
        ET_REL = 1;
        ET_EXEC = 2;
        ET_DYN = 3;
        ET_CORE = 4;
        ET_LOOS = 0xFE00;
        ET_HIOS = 0xFEFF;
        ET_LOPROC = 0xFF00;
        ET_HIPROC = 0xFFFF;
    };
    e_machine: uint16 {
        EM_NONE = 0;
        EM_M32 = 1;
        EM_SPARC = 2;
        EM_386 = 3;
        EM_68K = 4;
        EM_88K = 5;
        EM_860 = 7;
        EM_MIPS = 8;
        EM_X86_64 = 62;
    };
    e_version: uint32 {
        EV_NONE = 0;
        EV_CURRENT = 1;
    };
    e_entry: uint64;
    e_phoff: uint64;
    e_shoff: uint64;
    e_flags: uint32;
    e_ehsize: uint16;
    e_phentsize: uint16;
    e_phnum: uint16;
    e_shentsize: uint16;
    e_shnum: uint16;
    e_shstrndx: uint16;
    program_headers: array[e_phnum] of program_header;
    section_headers: array[e_shnum] of section_header;
}

program_header {
    p_type: uint32 {
        PT_NULL = 0;
        PT_LOAD = 1;
        PT_DYNAMIC = 2;
        PT_INTERP = 3;
        PT_NOTE = 4;
        PT_SHLIB = 5;
        PT_PHDR = 6;
        PT_TLS = 7;
        PT_LOOS = 0x60000000;
        PT_HIOS = 0x6FFFFFFF;
        PT_LOPROC = 0x70000000;
        PT_HIPROC = 0x7FFFFFFF;
    };
    p_flags: uint32 {
        PF_X = 0x1;
        PF_W = 0x2;
        PF_R = 0x4;
        PF_MASKOS = 0x0FF00000;
        PF_MASKPROC = 0xF0000000;
    };
    p_offset: uint64;
    p_vaddr: uint64;
    p_paddr: uint64;
    p_filesz: uint64;
    p_memsz: uint64;
    p_align: uint64;
}

section_header {
    sh_name: uint32;
    sh_type: uint32 {
        SHT_NULL = 0;
        SHT_PROGBITS = 1;
        SHT_SYMTAB = 2;
        SHT_STRTAB = 3;
        SHT_RELA = 4;
        SHT_HASH = 5;
        SHT_DYNAMIC = 6;
        SHT_NOTE = 7;
        SHT_NOBITS = 8;
        SHT_REL = 9;
        SHT_SHLIB = 10;
        SHT_DYNSYM = 11;
        SHT_INIT_ARRAY = 14;
        SHT_FINI_ARRAY = 15;
        SHT_PREINIT_ARRAY = 16;
        SHT_LOOS = 0x60000000;
        SHT_HIOS = 0x6FFFFFFF;
        SHT_LOPROC = 0x70000000;
        SHT_HIPROC = 0x7FFFFFFF;
    };
    sh_flags: uint64 {
        SHF_WRITE = 0x1;
        SHF_ALLOC = 0x2;
        SHF_EXECINSTR = 0x4;
        SHF_MASKOS = 0x0F000000;
        SHF_MASKPROC = 0xF0000000;
    };
    sh_addr: uint64;
    sh_offset: uint64;
    sh_size: uint64;
    sh_link: uint32;
    sh_info: uint32;
    sh_addralign: uint64;
    sh_entsize: uint64;
}