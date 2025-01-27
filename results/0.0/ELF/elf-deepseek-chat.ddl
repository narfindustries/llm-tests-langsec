struct ELFHeader {
    magic: byte[4] = { 0x7F, 'E', 'L', 'F' };
    class: byte;
    data: byte;
    version: byte;
    os_abi: byte;
    abi_version: byte;
    padding: byte[7];
    type: uint16;
    machine: uint16;
    version2: uint32;
    entry: uint64;
    phoff: uint64;
    shoff: uint64;
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
    flags: uint32;
    offset: uint64;
    vaddr: uint64;
    paddr: uint64;
    filesz: uint64;
    memsz: uint64;
    align: uint64;
}

struct SectionHeader {
    name: uint32;
    type: uint32;
    flags: uint64;
    addr: uint64;
    offset: uint64;
    size: uint64;
    link: uint32;
    info: uint32;
    addralign: uint64;
    entsize: uint64;
}

struct ELF {
    header: ELFHeader;
    program_headers: ProgramHeader[header.phnum];
    section_headers: SectionHeader[header.shnum];
}