module ELF;

import DAEDALUS::BitFields;
import NETWORK::Bytes;

type ELF32_Ehdr = struct {
  ei_mag     : Bytes::array 4;  // Magic number
  ei_class   : uint8;           // File class
  ei_data    : uint8;           // Data encoding
  ei_version : uint8;           // File version
  ei_osabi   : uint8;           // OS/ABI identification
  ei_abiversion: uint8;         // ABI version
  ei_pad     : Bytes::array 7;  // Padding
  e_type     : uint16;          // Object file type
  e_machine  : uint16;          // Machine type
  e_version  : uint32;          // Object file version
  e_entry    : uint32;          // Entry point address
  e_phoff    : uint32;          // Program header offset
  e_shoff    : uint32;          // Section header offset
  e_flags    : uint32;          // Processor-specific flags
  e_ehsize   : uint16;          // ELF header size
  e_phentsize: uint16;          // Program header entry size
  e_phnum    : uint16;          // Number of program headers
  e_shentsize: uint16;          // Section header entry size
  e_shnum    : uint16;          // Number of section headers
  e_shstrndx : uint16;          // Section name string table index
};

type ELF64_Ehdr = struct {
  ei_mag      : Bytes::array 4; // Magic number
  ei_class    : uint8;          // File class
  ei_data     : uint8;          // Data encoding
  ei_version  : uint8;          // File version
  ei_osabi    : uint8;          // OS/ABI identification
  ei_abiversion: uint8;         // ABI version
  ei_pad      : Bytes::array 7; // Padding
  e_type      : uint16;         // Object file type
  e_machine   : uint16;         // Machine type
  e_version   : uint32;         // Object file version
  e_entry     : uint64;         // Entry point address
  e_phoff     : uint64;         // Program header offset
  e_shoff     : uint64;         // Section header offset
  e_flags     : uint32;         // Processor-specific flags
  e_ehsize    : uint16;         // ELF header size
  e_phentsize : uint16;         // Program header entry size
  e_phnum     : uint16;         // Number of program headers
  e_shentsize : uint16;         // Section header entry size
  e_shnum     : uint16;         // Number of section headers
  e_shstrndx  : uint16;         // Section name string table index
};

root ELF_Header : union {
  1 : ELF32_Ehdr;
  2 : ELF64_Ehdr;
} using (BitFields::selector
        ((0, Bytes::decode (UTF8) == "\x7FELF"), // \x7F followed by "ELF"
         (4, uint8)));