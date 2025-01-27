type elf32_addr : uint32
type elf32_half : uint16
type elf32_off : uint32
type elf32_sword : int32
type elf32_word : uint32

struct Elf32_Ehdr {
    e_ident: [uint8; 16],
    e_type: elf32_half,
    e_machine: elf32_half,
    e_version: elf32_word,
    e_entry: elf32_addr,
    e_phoff: elf32_off,
    e_shoff: elf32_off,
    e_flags: elf32_word,
    e_ehsize: elf32_half,
    e_phentsize: elf32_half,
    e_phnum: elf32_half,
    e_shentsize: elf32_half,
    e_shnum: elf32_half,
    e_shstrndx: elf32_half
}

struct Elf32_Phdr {
    p_type: elf32_word,
    p_offset: elf32_off,
    p_vaddr: elf32_addr,
    p_paddr: elf32_addr,
    p_filesz: elf32_word,
    p_memsz: elf32_word,
    p_flags: elf32_word,
    p_align: elf32_word
}

struct Elf32_Shdr {
    sh_name: elf32_word,
    sh_type: elf32_word,
    sh_flags: elf32_word,
    sh_addr: elf32_addr,
    sh_offset: elf32_off,
    sh_size: elf32_word,
    sh_link: elf32_word,
    sh_info: elf32_word,
    sh_addralign: elf32_word,
    sh_entsize: elf32_word
}

enum Elf32_Phdr_Type : elf32_word {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6
}

enum Elf32_Shdr_Type : elf32_word {
    SHT_NULL = 0,
    SHT_PROGBITS = 1,
    SHT_SYMTAB = 2,
    SHT_STRTAB = 3,
    SHT_RELA = 4,
    SHT_HASH = 5,
    SHT_DYNAMIC = 6,
    SHT_NOTE = 7,
    SHT_NOBITS = 8,
    SHT_REL = 9,
    SHT_SHLIB = 10,
    SHT_DYNSYM = 11
}

fn parse_elf32(input: [uint8]) -> Result<(Elf32_Ehdr, [Elf32_Phdr], [Elf32_Shdr]), string> {
    let magic_bytes = [0x7F, 0x45, 0x4C, 0x46];
    
    if input[0..4] != magic_bytes {
        return Err("Invalid ELF magic bytes");
    }
    
    let header = Elf32_Ehdr {
        e_ident: input[0..16],
        e_type: read_uint16_le(input[16..18]),
        e_machine: read_uint16_le(input[18..20]),
        e_version: read_uint32_le(input[20..24]),
        e_entry: read_uint32_le(input[24..28]),
        e_phoff: read_uint32_le(input[28..32]),
        e_shoff: read_uint32_le(input[32..36]),
        e_flags: read_uint32_le(input[36..40]),
        e_ehsize: read_uint16_le(input[40..42]),
        e_phentsize: read_uint16_le(input[42..44]),
        e_phnum: read_uint16_le(input[44..46]),
        e_shentsize: read_uint16_le(input[46..48]),
        e_shnum: read_uint16_le(input[48..50]),
        e_shstrndx: read_uint16_le(input[50..52])
    };
    
    let program_headers = parse_program_headers(input, header.e_phoff, header.e_phnum, header.e_phentsize);
    let section_headers = parse_section_headers(input, header.e_shoff, header.e_shnum, header.e_shentsize);
    
    Ok((header, program_headers, section_headers))
}

fn parse_program_headers(input: [uint8], offset: elf32_off, num_headers: elf32_half, entry_size: elf32_half) -> [Elf32_Phdr] {
    let mut headers = [];
    
    for i in 0..num_headers {
        let start = offset + (i * entry_size);
        let end = start + entry_size;
        
        let header = Elf32_Phdr {
            p_type: read_uint32_le(input[start..start+4]),
            p_offset: read_uint32_le(input[start+4..start+8]),
            p_vaddr: read_uint32_le(input[start+8..start+12]),
            p_paddr: read_uint32_le(input[start+12..start+16]),
            p_filesz: read_uint32_le(input[start+16..start+20]),
            p_memsz: read_uint32_le(input[start+20..start+24]),
            p_flags: read_uint32_le(input[start+24..start+28]),
            p_align: read_uint32_le(input[start+28..start+32])
        };
        
        headers.push(header);
    }
    
    headers
}

fn parse_section_headers(input: [uint8], offset: elf32_off, num_headers: elf32_half, entry_size: elf32_half) -> [Elf32_Shdr] {
    let mut headers = [];
    
    for i in 0..num_headers {
        let start = offset + (i * entry_size);
        let end = start + entry_size;
        
        let header = Elf32_Shdr {
            sh_name: read_uint32_le(input[start..start+4]),
            sh_type: read_uint32_le(input[start+4..start+8]),
            sh_flags: read_uint32_le(input[start+8..start+12]),
            sh_addr: read_uint32_le(input[start+12..start+16]),
            sh_offset: read_uint32_le(input[start+16..start+20]),
            sh_size: read_uint32_le(input[start+20..start+24]),
            sh_link: read_uint32_le(input[start+24..start+28]),
            sh_info: read_uint32_le(input[start+28..start+32]),
            sh_addralign: read_uint32_le(input[start+32..start+36]),
            sh_entsize: read_uint32_le(input[start+36..start+40])
        };
        
        headers.push(header);
    }
    
    headers
}

fn read_uint16_le(bytes: [uint8]) -> elf32_half {
    (bytes[1] as elf32_half << 8) | (bytes[0] as elf32_half)
}

fn read_uint32_le(bytes: [uint8]) -> elf32_word {
    (bytes[3] as elf32_word << 24) | 
    (bytes[2] as elf32_word << 16) | 
    (bytes[1] as elf32_word << 8) | 
    (bytes[0] as elf32_word)
}