let elf_class = enum(u8) {
  ELFCLASSNONE = 0,
  ELFCLASS32 = 1,
  ELFCLASS64 = 2
}

let elf_data_encoding = enum(u8) {
  ELFDATANONE = 0,
  ELFDATA2LSB = 1,
  ELFDATA2MSB = 2
}

let elf_version = enum(u32) {
  EV_NONE = 0,
  EV_CURRENT = 1
}

let elf_osabi = enum(u8) {
  ELFOSABI_SYSV = 0,
  ELFOSABI_HPUX = 1,
  ELFOSABI_LINUX = 3,
  ELFOSABI_FREEBSD = 9,
  ELFOSABI_OPENBSD = 12
}

let elf_type = enum(u16) {
  ET_NONE = 0,
  ET_REL = 1,
  ET_EXEC = 2,
  ET_DYN = 3,
  ET_CORE = 4
}

let elf_machine = enum(u16) {
  EM_NONE = 0,
  EM_M32 = 1,
  EM_SPARC = 2,
  EM_386 = 3,
  EM_68K = 4,
  EM_88K = 5,
  EM_860 = 7,
  EM_MIPS = 8,
  EM_X86_64 = 62
}

let elf_header = {
  magic: [u8; 4] = [0x7F, 0x45, 0x4C, 0x46],
  class: elf_class,
  data_encoding: elf_data_encoding,
  version: elf_version,
  os_abi: elf_osabi,
  abi_version: u8,
  padding: [u8; 7],
  file_type: elf_type,
  machine: elf_machine,
  elf_version: u32,
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