define Elf = struct {
    header: ElfHeader,
    programHeaders: ProgramHeader[header.e_phnum],
    sectionHeaders: SectionHeader[header.e_shnum],
    symbolTables: optional SymbolTableEntry[],
    relocations: optional RelocationEntry[]
};

define ElfHeader = struct {
    e_ident: ElfIdent,
    e_type: u16,
    e_machine: u16,
    e_version: u32,
    e_entry: u64, // Adjusted for 64-bit
    e_phoff: u64,
    e_shoff: u64,
    e_flags: u32,
    e_ehsize: u16,
    e_phentsize: u16,
    e_phnum: u16,
    e_shentsize: u16,
    e_shnum: u16,
    e_shstrndx: u16
};

define ElfIdent = struct {
    magic: u32, // 0x7F 'E' 'L' 'F'
    class: u8,
    data: u8,
    version: u8,
    osabi: u8,
    abiversion: u8,
    pad: u8[7]
};

define ProgramHeader = struct {
    p_type: u32,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_flags: u32,
    p_align: u64
};

define SectionHeader = struct {
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64
};

define SymbolTableEntry = struct {
    st_name: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
    st_value: u64,
    st_size: u64
};

define RelocationEntry = struct {
    r_offset: u64,
    r_info: u64,
    r_addend: optional u64
};