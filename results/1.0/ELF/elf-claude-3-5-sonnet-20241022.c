#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

const HParser* ELF_Header_Parser() {
    // Magic number
    const HParser *magic = h_token((const uint8_t*)"\x7F""ELF", 4);
    
    // File class (32/64 bit)
    const HParser *ei_class = h_int_range(h_uint8(), 1, 2);
    
    // Data encoding
    const HParser *ei_data = h_int_range(h_uint8(), 1, 2);
    
    // Version
    const HParser *ei_version = h_uint8();
    
    // OS ABI
    const HParser *ei_osabi = h_uint8();
    
    // ABI Version
    const HParser *ei_abiversion = h_uint8();
    
    // Padding
    const HParser *padding = h_repeat_n(h_uint8(), 7);
    
    // File type
    const HParser *e_type = h_uint16();
    
    // Machine architecture
    const HParser *e_machine = h_uint16();
    
    // ELF version
    const HParser *e_version = h_uint32();
    
    // Entry point
    const HParser *e_entry = h_uint32();
    
    // Program header offset
    const HParser *e_phoff = h_uint32();
    
    // Section header offset
    const HParser *e_shoff = h_uint32();
    
    // Flags
    const HParser *e_flags = h_uint32();
    
    // Header size
    const HParser *e_ehsize = h_uint16();
    
    // Program header entry size
    const HParser *e_phentsize = h_uint16();
    
    // Number of program headers
    const HParser *e_phnum = h_uint16();
    
    // Section header entry size
    const HParser *e_shentsize = h_uint16();
    
    // Number of section headers
    const HParser *e_shnum = h_uint16();
    
    // Section header string table index
    const HParser *e_shstrndx = h_uint16();

    return h_sequence(magic, ei_class, ei_data, ei_version, ei_osabi,
                     ei_abiversion, padding, e_type, e_machine, e_version,
                     e_entry, e_phoff, e_shoff, e_flags, e_ehsize,
                     e_phentsize, e_phnum, e_shentsize, e_shnum,
                     e_shstrndx, NULL);
}

const HParser* init_parser() {
    return ELF_Header_Parser();
}