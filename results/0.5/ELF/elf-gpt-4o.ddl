// Improved Daedalus specification for ELF format

// ELF Header
struct ElfHeader {
    magic: u32; // 0x7F followed by "ELF"
    class: u8; // 1 = 32-bit, 2 = 64-bit
    data: u8; // 1 = little-endian, 2 = big-endian
    version: u8; // ELF version, should be 1
    os_abi: u8; // Target OS ABI
    abi_version: u8; // ABI version
    pad: u64; // Padding (should be zero)
    type: u16; // Object file type
    machine: u16; // Architecture
    version2: u32; // Version again, should be 1
    entry: u64; // Entry point address
    phoff: u64; // Program header table offset
    shoff: u64; // Section header table offset
    flags: u32; // Processor-specific flags
    ehsize: u16; // ELF header size
    phentsize: u16; // Program header entry size
    phnum: u16; // Number of program header entries
    shentsize: u16; // Section header entry size
    shnum: u16; // Number of section header entries
    shstrndx: u16; // Section header string table index
}

// Program Header
struct ProgramHeader {
    type: u32; // Segment type
    flags: u32; // Segment flags
    offset: u64; // Offset in the file
    vaddr: u64; // Virtual address in memory
    paddr: u64; // Physical address (if relevant)
    filesz: u64; // Size of the segment in the file
    memsz: u64; // Size of the segment in memory
    align: u64; // Alignment of the segment
}

// Section Header
struct SectionHeader {
    name: u32; // Offset to the section name in the string table
    type: u32; // Section type
    flags: u64; // Section flags
    addr: u64; // Virtual address in memory
    offset: u64; // Offset in the file
    size: u64; // Size of the section
    link: u32; // Link to another section
    info: u32; // Additional section information
    addralign: u64; // Section alignment
    entsize: u64; // Size of entries, if the section has a table
}

// ELF File
struct ElfFile {
    header: ElfHeader;
    program_headers: [ProgramHeader] (count: header.phnum);
    section_headers: [SectionHeader] (count: header.shnum);
}