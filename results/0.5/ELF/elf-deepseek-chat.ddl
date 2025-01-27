struct ELFHeader {
    magic: bytes[4] = [0x7F, 'E', 'L', 'F'];
    class: u8;
    data: u8;
    version: u8;
    os_abi: u8;
    abi_version: u8;
    padding: bytes[7];
    type: u16;
    machine: u16;
    version2: u32;
    entry: u64;
    phoff: u64;
    shoff: u64;
    flags: u32;
    ehsize: u16;
    phentsize: u16;
    phnum: u16;
    shentsize: u16;
    shnum: u16;
    shstrndx: u16;
}

struct ProgramHeader {
    type: u32;
    flags: u32;
    offset: u64;
    vaddr: u64;
    paddr: u64;
    filesz: u64;
    memsz: u64;
    align: u64;
}

struct SectionHeader {
    name: u32;
    type: u32;
    flags: u64;
    addr: u64;
    offset: u64;
    size: u64;
    link: u32;
    info: u32;
    addralign: u64;
    entsize: u64;
}

struct ELF {
    header: ELFHeader;
    program_headers: ProgramHeader[header.phnum];
    section_headers: SectionHeader[header.shnum];
}