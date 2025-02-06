type Elf32_Addr = uint32;
type Elf32_Off = uint32;
type Elf32_Half = uint16;
type Elf32_Word = uint32;
type Elf32_Sword = int32;

type Elf64_Addr = uint64;
type Elf64_Off = uint64;
type Elf64_Half = uint16;
type Elf64_Word = uint32;
type Elf64_Sword = int32;
type Elf64_Xword = uint64;
type Elf64_Sxword = int64;

struct Elf_Ident {
  uint32 ei_mag;         // Magic number, 0x7F454C46
  uint8 ei_class;        // File class
  uint8 ei_data;         // Data encoding
  uint8 ei_version;      // File version
  uint8 ei_osabi;        // OS/ABI identification
  uint8 ei_abiversion;   // ABI version
  bytes ei_pad;          // Padding bytes, 7 bytes
};

struct Elf32_Ehdr {
  Elf_Ident e_ident;  // ELF Identification
  Elf32_Half e_type;  // Object file type
  Elf32_Half e_machine;  // Machine type
  Elf32_Word e_version;  // Object file version
  Elf32_Addr e_entry;  // Entry point address
  Elf32_Off e_phoff;   // Program header offset
  Elf32_Off e_shoff;   // Section header offset
  Elf32_Word e_flags;  // Processor-specific flags
  Elf32_Half e_ehsize; // ELF header size
  Elf32_Half e_phentsize;  // Size of program header entry
  Elf32_Half e_phnum;  // Number of program header entries
  Elf32_Half e_shentsize;  // Size of section header entry
  Elf32_Half e_shnum;  // Number of section header entries
  Elf32_Half e_shstrndx;  // Section name string table index
};

struct Elf64_Ehdr {
  Elf_Ident e_ident;  // ELF Identification
  Elf64_Half e_type;  // Object file type
  Elf64_Half e_machine;  // Machine type
  Elf64_Word e_version;  // Object file version
  Elf64_Addr e_entry;  // Entry point address
  Elf64_Off e_phoff;   // Program header offset
  Elf64_Off e_shoff;   // Section header offset
  Elf64_Word e_flags;  // Processor-specific flags
  Elf64_Half e_ehsize; // ELF header size
  Elf64_Half e_phentsize;  // Size of program header entry
  Elf64_Half e_phnum;  // Number of program header entries
  Elf64_Half e_shentsize;  // Size of section header entry
  Elf64_Half e_shnum;  // Number of section header entries
  Elf64_Half e_shstrndx;  // Section name string table index
};

struct Elf32_Phdr {
  Elf32_Word p_type;   // Type of segment
  Elf32_Off p_offset;  // Offset in file
  Elf32_Addr p_vaddr;  // Virtual address in memory
  Elf32_Addr p_paddr;  // Reserved
  Elf32_Word p_filesz; // Size of segment in file
  Elf32_Word p_memsz;  // Size of segment in memory
  Elf32_Word p_flags;  // Segment flags
  Elf32_Word p_align;  // Segment alignment
};

struct Elf64_Phdr {
  Elf64_Word p_type;   // Type of segment
  Elf64_Word p_flags;  // Segment flags
  Elf64_Off p_offset;  // Offset in file
  Elf64_Addr p_vaddr;  // Virtual address in memory
  Elf64_Addr p_paddr;  // Reserved
  Elf64_Xword p_filesz; // Size of segment in file
  Elf64_Xword p_memsz;  // Size of segment in memory
  Elf64_Xword p_align;  // Segment alignment
};

struct Elf32_Shdr {
  Elf32_Word sh_name;  // Section name (index into the section header string table)
  Elf32_Word sh_type;  // Section type
  Elf32_Word sh_flags; // Section flags
  Elf32_Addr sh_addr;  // Address in memory
  Elf32_Off sh_offset; // Offset in file
  Elf32_Word sh_size;  // Size of section
  Elf32_Word sh_link;  // Link to another section
  Elf32_Word sh_info;  // Additional section information
  Elf32_Word sh_addralign; // Section alignment
  Elf32_Word sh_entsize; // Entry size if section holds table
};

struct Elf64_Shdr {
  Elf64_Word sh_name;  // Section name (index into the section header string table)
  Elf64_Word sh_type;  // Section type
  Elf64_Xword sh_flags; // Section flags
  Elf64_Addr sh_addr;  // Address in memory
  Elf64_Off sh_offset; // Offset in file
  Elf64_Xword sh_size;  // Size of section
  Elf64_Word sh_link;  // Link to another section
  Elf64_Word sh_info;  // Additional section information
  Elf64_Xword sh_addralign; // Section alignment
  Elf64_Xword sh_entsize; // Entry size if section holds table
};