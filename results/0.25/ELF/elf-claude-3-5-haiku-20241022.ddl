type ELF_File = {
    header: ELF_Header,
    program_headers: [Program_Header],
    section_headers: [Section_Header],
    sections: [Section]
}

type ELF_Header = {
    magic: [u8; 4],  // ELF magic number
    class: u8,       // 32-bit or 64-bit 
    data_encoding: u8, 
    version: u8,
    os_abi: u8,
    abi_version: u8,
    padding: [u8; 7],
    file_type: u16,
    machine: u16,
    elf_version: u32,
    entry_point: u64,
    program_header_offset: u64,
    section_header_offset: u64,
    flags: u32,
    header_size: u16,
    program_header_entry_size: u16,
    program_header_count: u16,
    section_header_entry_size: u16,
    section_header_count: u16,
    section_names_index: u16
}

type Program_Header = {
    type: u32,
    flags: u32,
    offset: u64,
    virtual_address: u64,
    physical_address: u64,
    file_size: u64,
    memory_size: u64,
    alignment: u64
}

type Section_Header = {
    name_offset: u32,
    type: u32,
    flags: u64,
    virtual_address: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    address_alignment: u64,
    entry_size: u64
}

type Section = {
    name: string,
    data: [u8]
}

parse ELF_File = {
    header: ELF_Header,
    program_headers: repeat(Program_Header, header.program_header_count),
    section_headers: repeat(Section_Header, header.section_header_count),
    sections: map(section_headers, |sh| {
        Section {
            name: read_section_name(sh),
            data: read_section_data(sh)
        }
    })
}