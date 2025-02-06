ELF: struct {
    e_ident: struct {
        EI_MAG0: uint8,
        EI_MAG1: uint8,
        EI_MAG2: uint8,
        EI_MAG3: uint8,
        EI_CLASS: enum("ELFCLASSNONE"=0, "ELFCLASS32"=1, "ELFCLASS64"=2),
        EI_DATA: enum("ELFDATANONE"=0, "ELFDATA2LSB"=1, "ELFDATA2MSB"=2),
        EI_VERSION: enum("EV_NONE"=0, "EV_CURRENT"=1),
        EI_OSABI: uint8,
        EI_ABIVERSION: uint8,
        EI_PAD: array[7] of uint8
    },
    e_type: enum("ET_NONE"=0, "ET_REL"=1, "ET_EXEC"=2, "ET_DYN"=3, "ET_CORE"=4),
    e_machine: uint16,
    e_version: enum("EV_NONE"=0, "EV_CURRENT"=1),
    e_entry: uint32,
    e_phoff: uint32,
    e_shoff: uint32,
    e_flags: uint32,
    e_ehsize: uint16,
    e_phentsize: uint16,
    e_phnum: uint16,
    e_shentsize: uint16,
    e_shnum: uint16,
    e_shstrndx: uint16,
    phdrs: array[e_phnum] of struct {
        p_type: enum("PT_NULL"=0, "PT_LOAD"=1, "PT_DYNAMIC"=2, "PT_INTERP"=3, "PT_NOTE"=4, "PT_SHLIB"=5, "PT_PHDR"=6),
        p_offset: uint32,
        p_vaddr: uint32,
        p_paddr: uint32,
        p_filesz: uint32,
        p_memsz: uint32,
        p_flags: uint32,
        p_align: uint32
    },
    shdrs: array[e_shnum] of struct {
        sh_name: uint32,
        sh_type: enum("SHT_NULL"=0, "SHT_PROGBITS"=1, "SHT_SYMTAB"=2, "SHT_STRTAB"=3, "SHT_RELA"=4, "SHT_HASH"=5, "SHT_DYNAMIC"=6, "SHT_NOTE"=7, "SHT_NOBITS"=8, "SHT_REL"=9, "SHT_SHLIB"=10, "SHT_DYNSYM"=11),
        sh_flags: uint32,
        sh_addr: uint32,
        sh_offset: uint32,
        sh_size: uint32,
        sh_link: uint32,
        sh_info: uint32,
        sh_addralign: uint32,
        sh_entsize: uint32
    },
    symtab: array[] of struct {
        st_name: uint32,
        st_value: uint32,
        st_size: uint32,
        st_info: struct {
            bind: enum("STB_LOCAL"=0, "STB_GLOBAL"=1, "STB_WEAK"=2),
            type: enum("STT_NOTYPE"=0, "STT_OBJECT"=1, "STT_FUNC"=2, "STT_SECTION"=3, "STT_FILE"=4)
        },
        st_other: enum("STV_DEFAULT"=0, "STV_INTERNAL"=1, "STV_HIDDEN"=2, "STV_PROTECTED"=3),
        st_shndx: uint16
    },
    rela: array[] of struct {
        r_offset: uint32,
        r_info: struct {
            sym: uint32,
            type: uint32
        },
        r_addend: uint32
    },
    dynamic: array[] of struct {
        d_tag: enum("DT_NULL"=0, "DT_NEEDED"=1, "DT_PLTRELSZ"=2, "DT_PLTGOT"=3, "DT_HASH"=4, "DT_STRTAB"=5, "DT_SYMTAB"=6, "DT_RELA"=7, "DT_RELASZ"=8, "DT_RELAENT"=9),
        d_un: uint32
    },
    note: struct {
        n_namesz: uint32,
        n_descsz: uint32,
        n_type: enum("NT_PRSTATUS"=1, "NT_FPREGSET"=2, "NT_PRPSINFO"=3)
    }
}