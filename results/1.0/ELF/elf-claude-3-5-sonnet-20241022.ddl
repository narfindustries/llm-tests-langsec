Grammar {

let EI_NIDENT = 16
let EI_CLASS_NONE = 0
let EI_CLASS_32   = 1
let EI_CLASS_64   = 2
let EI_DATA_NONE = 0
let EI_DATA_2LSB = 1
let EI_DATA_2MSB = 2
let ET_NONE = 0
let ET_REL  = 1
let ET_EXEC = 2
let ET_DYN  = 3
let ET_CORE = 4

ELF_Ident = {
    magic      : UINT8[4]   -- 0x7F 'E' 'L' 'F'
    class      : UINT8
    data       : UINT8
    version    : UINT8
    osabi      : UINT8
    abiversion : UINT8
    pad        : UINT8[7]
}

Elf32_Ehdr = {
    ident      : ELF_Ident
    type       : UINT16
    machine    : UINT16
    version    : UINT32
    entry      : UINT32
    phoff      : UINT32
    shoff      : UINT32
    flags      : UINT32
    ehsize     : UINT16
    phentsize  : UINT16
    phnum      : UINT16
    shentsize  : UINT16
    shnum      : UINT16
    shstrndx   : UINT16
}

Elf64_Ehdr = {
    ident      : ELF_Ident
    type       : UINT16
    machine    : UINT16
    version    : UINT32
    entry      : UINT64
    phoff      : UINT64
    shoff      : UINT64
    flags      : UINT32
    ehsize     : UINT16
    phentsize  : UINT16
    phnum      : UINT16
    shentsize  : UINT16
    shnum      : UINT16
    shstrndx   : UINT16
}

Elf32_Phdr = {
    type       : UINT32
    offset     : UINT32
    vaddr      : UINT32
    paddr      : UINT32
    filesz     : UINT32
    memsz      : UINT32
    flags      : UINT32
    align      : UINT32
}

Elf64_Phdr = {
    type       : UINT32
    flags      : UINT32
    offset     : UINT64
    vaddr      : UINT64
    paddr      : UINT64
    filesz     : UINT64
    memsz      : UINT64
    align      : UINT64
}

Elf32_Shdr = {
    name       : UINT32
    type       : UINT32
    flags      : UINT32
    addr       : UINT32
    offset     : UINT32
    size       : UINT32
    link       : UINT32
    info       : UINT32
    addralign  : UINT32
    entsize    : UINT32
}

Elf64_Shdr = {
    name       : UINT32
    type       : UINT32
    flags      : UINT64
    addr       : UINT64
    offset     : UINT64
    size       : UINT64
    link       : UINT32
    info       : UINT32
    addralign  : UINT64
    entsize    : UINT64
}

ELF = {
    ident : ELF_Ident
    switch (ident.class) {
        case EI_CLASS_32:
            header32 : Elf32_Ehdr
            pheaders : Elf32_Phdr[header32.phnum]
            sheaders : Elf32_Shdr[header32.shnum]
        case EI_CLASS_64:
            header64 : Elf64_Ehdr
            pheaders : Elf64_Phdr[header64.phnum]
            sheaders : Elf64_Shdr[header64.shnum]
    }
}

Main = ELF
}