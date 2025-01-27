domain ELF {
  type uint8 = uint8;
  type uint16 = uint16;
  type uint32 = uint32;
  type uint64 = uint64;
  type int8 = int8;
  type int16 = int16;
  type int32 = int32;
  type int64 = int64;

  type Elf64_Addr = uint64;
  type Elf64_Off = uint64;
  type Elf64_Half = uint16;
  type Elf64_Word = uint32;
  type Elf64_Sword = int32;
  type Elf64_Xword = uint64;
  type Elf64_Sxword = int64;

  type Elf64_Ehdr = struct {
    e_ident: array[16] of uint8,
    e_type: Elf64_Half,
    e_machine: Elf64_Half,
    e_version: Elf64_Word,
    e_entry: Elf64_Addr,
    e_phoff: Elf64_Off,
    e_shoff: Elf64_Off,
    e_flags: Elf64_Word,
    e_ehsize: Elf64_Half,
    e_phentsize: Elf64_Half,
    e_phnum: Elf64_Half,
    e_shentsize: Elf64_Half,
    e_shnum: Elf64_Half,
    e_shstrndx: Elf64_Half
  };

  type Elf64_Phdr = struct {
    p_type: Elf64_Word,
    p_flags: Elf64_Word,
    p_offset: Elf64_Off,
    p_vaddr: Elf64_Addr,
    p_paddr: Elf64_Addr,
    p_filesz: Elf64_Xword,
    p_memsz: Elf64_Xword,
    p_align: Elf64_Xword
  };

  type Elf64_Shdr = struct {
    sh_name: Elf64_Word,
    sh_type: Elf64_Word,
    sh_flags: Elf64_Xword,
    sh_addr: Elf64_Addr,
    sh_offset: Elf64_Off,
    sh_size: Elf64_Xword,
    sh_link: Elf64_Word,
    sh_info: Elf64_Word,
    sh_addralign: Elf64_Xword,
    sh_entsize: Elf64_Xword
  };

  type Elf64_Sym = struct {
    st_name: Elf64_Word,
    st_info: uint8,
    st_other: uint8,
    st_shndx: Elf64_Half,
    st_value: Elf64_Addr,
    st_size: Elf64_Xword
  };

  grammar TOP {
    elf: ehdr phdrs shdrs,
    ehdr: Elf64_Ehdr,
    phdrs: array[e_phnum] of Elf64_Phdr,
    shdrs: array[e_shnum] of Elf64_Shdr
  };

  start grammar TOP;
}