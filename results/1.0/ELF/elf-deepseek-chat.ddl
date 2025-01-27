struct ELFHeader {
    magic: byte[4] = {0x7F, 'E', 'L', 'F'};
    class: byte;
    data: byte;
    version: byte;
    os_abi: byte;
    abi_version: byte;
    padding: byte[7];
    type: uint16;
    machine: uint16;
    version2: uint32;
    entry: uint32;
    phoff: uint32;
    shoff: uint32;
    flags: uint32;
    ehsize: uint16;
    phentsize: uint16;
    phnum: uint16;
    shentsize: uint16;
    shnum: uint16;
    shstrndx: uint16;
}

struct ProgramHeader {
    type: uint32;
    offset: uint32;
    vaddr: uint32;
    paddr: uint32;
    filesz: uint32;
    memsz: uint32;
    flags: uint32;
    align: uint32;
}

struct SectionHeader {
    name: uint32;
    type: uint32;
    flags: uint32;
    addr: uint32;
    offset: uint32;
    size: uint32;
    link: uint32;
    info: uint32;
    addralign: uint32;
    entsize: uint32;
}

struct ELF {
    header: ELFHeader;
    program_headers: ProgramHeader[header.phnum];
    section_headers: SectionHeader[header.shnum];
}