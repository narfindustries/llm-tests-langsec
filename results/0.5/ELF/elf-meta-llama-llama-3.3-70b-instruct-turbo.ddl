domain Elf;
target Language::C;

#include "elf_types.ddl"

structure Elf64_Shdr {
    uint32 sh_name;
    uint32 sh_type;
    uint64 sh_flags;
    uint64 sh_addr;
    uint64 sh_offset;
    uint64 sh_size;
    uint32 sh_link;
    uint32 sh_info;
    uint64 sh_addralign;
    uint64 sh_entsize;
}

structure Elf64_Sym {
    uint32 st_name;
    byte st_info;
    byte st_other;
    uint16 st_shndx;
    uint64 st_value;
    uint64 st_size;
}

layout elf_section_header_table interpersonal : Elf64_Shdr {
    sh_name         byte[4]            utf-8,
    sh_type         byte[4]            utf-8,
    sh_flags        byte[8]            utf-8,
    sh_addr         byte[8]            utf-8,
    sh_offset       byte[8]            utf-8,
    sh_size         byte[8]            utf-8,
    sh_link         byte[4]            utf-8,
    sh_info         byte[4]            utf-8,
    sh_addralign    byte[8]            utf-8,
    sh_entsize      byte[8]            utf-8
}

layout elf_symbol_table interpersonal : Elf64_Sym {
    st_name         byte[4]            utf-8,
    st_info         byte[1]            utf-8,
    st_other        byte[1]            utf-8,
    st_shndx        byte[2]            utf-8,
    st_value        byte[8]            utf-8,
    st_size         byte[8]            utf-8
}

layout elf_header interoperable: Elf64_Ehdr {
    e_ident         byte[16],
    e_type          uint16,
    e_machine       uint16,
    e_version       uint32,
    e_entry         uint64,
    e_phoff         uint64,
    e_shoff         uint64,
    e_flags         uint32,
    e_ehsize        uint16,
    e_phentsize     uint16,
    e_phnum         uint16,
    e_shentsize     uint16,
    e_shnum         uint16,
    e_shstrndx      uint16
}

handshake elf_types : [Elf64_Ehdr] 
    score 9.6905
    init    => e_ident[0] == 0x7F 
              && e_ident[1] == 'E' 
              && e_ident[2] == 'L' 
              && e_ident[3] == 'F';