element Elf32_Ehdr {
  uint8[16] e_ident
  uint16 e_type
  uint16 e_machine
  uint32 e_version
  uint32 e_entry
  uint32 e_phoff
  uint32 e_shoff
  uint32 e_flags
  uint16 e_ehsize
  uint16 e_phentsize
  uint16 e_phnum
  uint16 e_shentsize
  uint16 e_shnum
  uint16 e_shstrndx
}

element Elf64_Ehdr {
  uint8[16] e_ident
  uint16 e_type
  uint16 e_machine
  uint32 e_version
  uint64 e_entry
  uint64 e_phoff
  uint64 e_shoff
  uint32 e_flags
  uint16 e_ehsize
  uint16 e_phentsize
  uint16 e_phnum
  uint16 e_shentsize
  uint16 e_shnum
  uint16 e_shstrndx
}

element Elf32_Phdr {
  uint32 p_type
  uint32 p_offset
  uint32 p_vaddr
  uint32 p_paddr
  uint32 p_filesz
  uint32 p_memsz
  uint32 p_flags
  uint32 p_align
}

element Elf64_Phdr {
  uint32 p_type
  uint32 p_flags
  uint64 p_offset
  uint64 p_vaddr
  uint64 p_paddr
  uint64 p_filesz
  uint64 p_memsz
  uint64 p_align
}

element Elf32_Shdr {
  uint32 sh_name
  uint32 sh_type
  uint32 sh_flags
  uint32 sh_addr
  uint32 sh_offset
  uint32 sh_size
  uint32 sh_link
  uint32 sh_info
  uint32 sh_addralign
  uint32 sh_entsize
}

element Elf64_Shdr {
  uint32 sh_name
  uint32 sh_type
  uint64 sh_flags
  uint64 sh_addr
  uint64 sh_offset
  uint64 sh_size
  uint32 sh_link
  uint32 sh_info
  uint64 sh_addralign
  uint64 sh_entsize
}

element Elf32_Sym {
  uint32 st_name
  uint32 st_value
  uint32 st_size
  uint8 st_info
  uint8 st_other
  uint16 st_shndx
}

element Elf64_Sym {
  uint32 st_name
  uint8 st_info
  uint8 st_other
  uint16 st_shndx
  uint64 st_value
  uint64 st_size
}

element Elf32_Rel {
  uint32 r_offset
  uint32 r_info
}

element Elf64_Rel {
  uint64 r_offset
  uint64 r_info
}

element Elf32_Rela {
  uint32 r_offset
  uint32 r_info
  int32 r_addend
}

element Elf64_Rela {
  uint64 r_offset
  uint64 r_info
  int64 r_addend
}

enum Elf32_Phdr_Types {
  PT_NULL = 0
  PT_LOAD = 1
  PT_DYNAMIC = 2
  PT_INTERP = 3
  PT_NOTE = 4
  PT_SHLIB = 5
  PT_PHDR = 6
  PT_TLS = 7
}

enum Elf64_Phdr_Types {
  PT_NULL = 0
  PT_LOAD = 1
  PT_DYNAMIC = 2
  PT_INTERP = 3
  PT_NOTE = 4
  PT_SHLIB = 5
  PT_PHDR = 6
  PT_TLS = 7
}

enum Elf32_Shdr_Types {
  SHT_NULL = 0
  SHT_PROGBITS = 1
  SHT_SYMTAB = 2
  SHT_STRTAB = 3
  SHT_RELA = 4
  SHT_HASH = 5
  SHT_DYNAMIC = 6
  SHT_NOTE = 7
  SHT_NOBITS = 8
  SHT_REL = 9
  SHT_SHLIB = 10
  SHT_DYNSYM = 11
}

enum Elf64_Shdr_Types {
  SHT_NULL = 0
  SHT_PROGBITS = 1
  SHT_SYMTAB = 2
  SHT_STRTAB = 3
  SHT_RELA = 4
  SHT_HASH = 5
  SHT_DYNAMIC = 6
  SHT_NOTE = 7
  SHT_NOBITS = 8
  SHT_REL = 9
  SHT_SHLIB = 10
  SHT_DYNSYM = 11
}

enum Elf32_Rel_Types {
  R_386_NONE = 0
  R_386_32 = 1
  R_386_PC32 = 2
  R_386_GOT32 = 3
  R_386_PLT32 = 4
  R_386_COPY = 5
  R_386_GLOB_DAT = 6
  R_386_JMP_SLOT = 7
  R_386_RELATIVE = 8
  R_386_GOTOFF = 9
  R_386_GOTPC = 10
}

enum Elf64_Rel_Types {
  R_X86_64_NONE = 0
  R_X86_64_64 = 1
  R_X86_64_PC32 = 2
  R_X86_64_GOT32 = 3
  R_X86_64_PLT32 = 4
  R_X86_64_COPY = 5
  R_X86_64_GLOB_DAT = 6
  R_X86_64_JMP_SLOT = 7
  R_X86_64_RELATIVE = 8
  R_X86_64_GOTPCREL = 9
  R_X86_64_32 = 10
  R_X86_64_32S = 11
  R_X86_64_64PC = 12
}

enum Elf32_Ei_Class {
  ELFCLASS32 = 1
  ELFCLASS64 = 2
}

enum Elf32_Ei_Data {
  ELFDATA2LSB = 1
  ELFDATA2MSB = 2
}

enum Elf32_Ei_Version {
  EV_CURRENT = 1
}

enum Elf32_Ei_Osabi {
  ELFOSABI_NONE = 0
  ELFOSABI_HPUX = 1
  ELFOSABI_NETBSD = 2
  ELFOSABI_LINUX = 3
  ELFOSABI_SOLARIS = 6
  ELFOSABI_AIX = 7
  ELFOSABI_IRIX = 9
  ELFOSABI_FREEBSD = 10
  ELFOSABI_TRU64 = 11
  ELFOSABI_MODESTO = 12
  ELFOSABI_OPENBSD = 13
  ELFOSABI_OPENVMS = 14
  ELFOSABI_NSK = 15
  ELFOSABI_AROS = 16
}

type elf_file {
  choice {
    elf32: Elf32_Ehdr
    elf64: Elf64_Ehdr
  }
  array[header.e_phnum] of {
    choice {
      elf32: Elf32_Phdr
      elf64: Elf64_Phdr
    }
  }
  array[header.e_shnum] of {
    choice {
      elf32: Elf32_Shdr
      elf64: Elf64_Shdr
    }
  }
}