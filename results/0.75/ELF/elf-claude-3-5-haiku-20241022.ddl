type header = {
    magic: [4]u8,
    class: u8,
    data: u8,
    version: u8,
    os_abi: u8,
    abi_version: u8,
    pad: [8]u8
}

type section_header = {
    name_index: u32,
    type: u32,
    flags: u64,
    addr: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addr_align: u64,
    entry_size: u64
}

type elf_file = {
    header: header,
    section_headers: [*]section_header
}

pub fn parse(input: [*]u8) -> elf_file {
    let header = std.read_struct<header>(input);
    
    if header.magic != [0x7F, 0x45, 0x4C, 0x46] {
        std.panic("Invalid ELF magic number");
    }

    let section_header_count = std.read_u16(input + 0x3C);
    let section_header_offset = std.read_u64(input + 0x28);
    
    let section_headers = std.read_array<section_header>(
        input + section_header_offset, 
        section_header_count
    );

    return {
        header: header,
        section_headers: section_headers
    };
}