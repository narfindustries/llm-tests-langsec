module ELF {

  import utils;

  // Define the ELF header
  struct ElfHeader {
    e_ident: array [u8] (16);  // ELF identification
    e_type: u16;               // Object file type
    e_machine: u16;            // Machine type
    e_version: u32;            // Object file version
    e_entry: u64;              // Entry point address
    e_phoff: u64;              // Program header offset
    e_shoff: u64;              // Section header offset
    e_flags: u32;              // Processor-specific flags
    e_ehsize: u16;             // ELF header size
    e_phentsize: u16;          // Size of one program header entry
    e_phnum: u16;              // Number of program header entries
    e_shentsize: u16;          // Size of one section header entry
    e_shnum: u16;              // Number of section header entries
    e_shstrndx: u16;           // Section header string table index
  }

  // Define the Program Header
  struct ProgramHeader {
    p_type: u32;               // Type of segment
    p_flags: u32;              // Segment attributes
    p_offset: u64;             // Segment offset in file
    p_vaddr: u64;              // Virtual address in memory
    p_paddr: u64;              // Reserved
    p_filesz: u64;             // Size of segment in file
    p_memsz: u64;              // Size of segment in memory
    p_align: u64;              // Segment alignment
  }

  // Define the Section Header
  struct SectionHeader {
    sh_name: u32;              // Section name (index into the section header string table)
    sh_type: u32;              // Section type
    sh_flags: u64;             // Section attributes
    sh_addr: u64;              // Virtual address in memory
    sh_offset: u64;            // Offset in file
    sh_size: u64;              // Size of section
    sh_link: u32;              // Link to other section
    sh_info: u32;              // Additional section information
    sh_addralign: u64;         // Section alignment
    sh_entsize: u64;           // Entry size if section holds table
  }

  // Parse the ELF file
  struct ELF_File {
    header: ElfHeader;
    programHeaders: array [ProgramHeader] (header.e_phnum);
    sectionHeaders: array [SectionHeader] (header.e_shnum);
  }

  // Utility functions for parsing
  namespace utils {
    // Example utility function
    def exampleUtilityFunction(x: u32): u32 = x + 1;
  }
}