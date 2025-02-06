grammar ELF {
  enum Class : u8 {
    None = 0x00,
    Elf32 = 0x01,
    Elf64 = 0x02
  }

  enum DataEncoding : u8 {
    None = 0x00,
    LittleEndian = 0x01,
    BigEndian = 0x02
  }

  enum Version : u8 {
    None = 0x00,
    Current = 0x01
  }

  enum OSABI : u8 {
    SystemV = 0x00,
    HPUX = 0x01,
    NetBSD = 0x02,
    Linux = 0x03,
    Solaris = 0x06,
    FreeBSD = 0x09,
    ARM = 0x0C
  }

  enum FileType : u16 {
    None = 0x00,
    Relocatable = 0x01,
    Executable = 0x02,
    SharedObject = 0x03,
    Core = 0x04
  }

  enum Machine : u16 {
    None = 0x00,
    SPARC = 0x02,
    x86 = 0x03,
    MIPS = 0x08,
    PowerPC = 0x14,
    ARM = 0x28,
    x86_64 = 0x3E
  }

  struct Header {
    magic: [u8; 4],
    class: Class,
    encoding: DataEncoding,
    version: Version,
    os_abi: OSABI,
    abi_version: u8,
    padding: [u8; 7],
    type: FileType,
    machine: Machine,
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
    section_header_string_index: u16
  }

  start: Header = {
    let magic = input[0:4];
    let class = input[4] as Class;
    let encoding = input[5] as DataEncoding;
    let version = input[6] as Version;
    let os_abi = input[7] as OSABI;
    let abi_version = input[8];
    let padding = input[9:16];
    let type = read_u16(input[16:18]) as FileType;
    let machine = read_u16(input[18:20]) as Machine;
    let elf_version = read_u32(input[20:24]);
    let entry_point = read_u64(input[24:32]);
    let program_header_offset = read_u64(input[32:40]);
    let section_header_offset = read_u64(input[40:48]);
    let flags = read_u32(input[48:52]);
    let header_size = read_u16(input[52:54]);
    let program_header_entry_size = read_u16(input[54:56]);
    let program_header_count = read_u16(input[56:58]);
    let section_header_entry_size = read_u16(input[58:60]);
    let section_header_count = read_u16(input[60:62]);
    let section_header_string_index = read_u16(input[62:64]);

    {
      magic,
      class,
      encoding,
      version,
      os_abi,
      abi_version,
      padding,
      type,
      machine,
      elf_version,
      entry_point,
      program_header_offset,
      section_header_offset,
      flags,
      header_size,
      program_header_entry_size,
      program_header_count,
      section_header_entry_size,
      section_header_count,
      section_header_string_index
    }
  }
}