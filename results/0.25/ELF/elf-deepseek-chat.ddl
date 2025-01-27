struct ELFHeader {
    magic: U8[4] = [0x7F, 'E', 'L', 'F'];
    class: U8;
    data: U8;
    version: U8;
    os_abi: U8;
    abi_version: U8;
    padding: U8[7];
    type: U16;
    machine: U16;
    version2: U32;
    entry: U32;
    phoff: U32;
    shoff: U32;
    flags: U32;
    ehsize: U16;
    phentsize: U16;
    phnum: U16;
    shentsize: U16;
    shnum: U16;
    shstrndx: U16;
}

struct ProgramHeader {
    type: U32;
    offset: U32;
    vaddr: U32;
    paddr: U32;
    filesz: U32;
    memsz: U32;
    flags: U32;
    align: U32;
}

struct SectionHeader {
    name: U32;
    type: U32;
    flags: U32;
    addr: U32;
    offset: U32;
    size: U32;
    link: U32;
    info: U32;
    addralign: U32;
    entsize: U32;
}

struct ELF {
    header: ELFHeader;
    program_headers: ProgramHeader[header.phnum];
    section_headers: SectionHeader[header.shnum];
}