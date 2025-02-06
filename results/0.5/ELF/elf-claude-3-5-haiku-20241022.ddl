enum ElfClass : u8 {
  ELFCLASSNONE = 0,
  ELFCLASS32 = 1,
  ELFCLASS64 = 2
}

enum ElfDataEncoding : u8 {
  ELFDATANONE = 0,
  ELFDATA2LSB = 1,
  ELFDATA2MSB = 2
}

enum ElfVersion : u8 {
  EV_NONE = 0,
  EV_CURRENT = 1
}

enum ElfOsAbi : u8 {
  ELFOSABI_SYSV = 0,
  ELFOSABI_HPUX = 1,
  ELFOSABI_LINUX = 3,
  ELFOSABI_FREEBSD = 9,
  ELFOSABI_OPENBSD = 12
}

enum ElfType : u16 {
  ET_NONE = 0,
  ET_REL = 1,
  ET_EXEC = 2,
  ET_DYN = 3,
  ET_CORE = 4
}

enum ElfMachine : u16 {
  EM_NONE = 0,
  EM_M32 = 1,
  EM_SPARC = 2,
  EM_386 = 3,
  EM_68K = 4,
  EM_88K = 5,
  EM_860 = 7,
  EM_X86_64 = 62
}

struct ElfIdentification {
  magic: [u8; 4],
  class: ElfClass,
  data_encoding: ElfDataEncoding,
  version: ElfVersion,
  os_abi: ElfOsAbi,
  abi_version: u8,
  padding: [u8; 7]
}

struct Elf32Header {
  identification: ElfIdentification,
  type: ElfType,
  machine: ElfMachine,
  version: u32,
  entry_point: u32,
  program_header_offset: u32,
  section_header_offset: u32,
  flags: u32,
  header_size: u16,
  program_header_entry_size: u16,
  program_header_num_entries: u16,
  section_header_entry_size: u16,
  section_header_num_entries: u16,
  section_header_string_table_index: u16
}

struct Elf64Header {
  identification: ElfIdentification,
  type: ElfType,
  machine: ElfMachine,
  version: u32,
  entry_point: u64,
  program_header_offset: u64,
  section_header_offset: u64,
  flags: u32,
  header_size: u16,
  program_header_entry_size: u16,
  program_header_num_entries: u16,
  section_header_entry_size: u16,
  section_header_num_entries: u16,
  section_header_string_table_index: u16
}

parser ElfParser {
  header: Elf32Header | Elf64Header;

  parse {
    header = match identification.class {
      ElfClass::ELFCLASS32 => Elf32Header,
      ElfClass::ELFCLASS64 => Elf64Header
    }
  }
}