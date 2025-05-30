grammar ELF;

import std.core;

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

type Elf_Ident = struct {
  magic: bytes[4] = "\x7FELF";
  class: uint8; // 1 = 32-bit, 2 = 64-bit
  data: uint8; // 1 = little endian, 2 = big endian
  version: uint8;
  osabi: uint8;
  abiversion: uint8;
  pad: bytes[7];
};

type Elf32_Ehdr = struct {
  ident: Elf_Ident;
  type: Elf32_Half;
  machine: Elf32_Half;
  version: Elf32_Word;
  entry: Elf32_Addr;
  phoff: Elf32_Off;
  shoff: Elf32_Off;
  flags: Elf32_Word;
  ehsize: Elf32_Half;
  phentsize: Elf32_Half;
  phnum: Elf32_Half;
  shentsize: Elf32_Half;
  shnum: Elf32_Half;
  shstrndx: Elf32_Half;
};

type Elf64_Ehdr = struct {
  ident: Elf_Ident;
  type: Elf64_Half;
  machine: Elf64_Half;
  version: Elf64_Word;
  entry: Elf64_Addr;
  phoff: Elf64_Off;
  shoff: Elf64_Off;
  flags: Elf64_Word;
  ehsize: Elf64_Half;
  phentsize: Elf64_Half;
  phnum: Elf64_Half;
  shentsize: Elf64_Half;
  shnum: Elf64_Half;
  shstrndx: Elf64_Half;
};

type Elf32_Shdr = struct {
  name: Elf32_Word;
  type: Elf32_Word;
  flags: Elf32_Word;
  addr: Elf32_Addr;
  offset: Elf32_Off;
  size: Elf32_Word;
  link: Elf32_Word;
  info: Elf32_Word;
  addralign: Elf32_Word;
  entsize: Elf32_Word;
};

type Elf64_Shdr = struct {
  name: Elf64_Word;
  type: Elf64_Word;
  flags: Elf64_Xword;
  addr: Elf64_Addr;
  offset: Elf64_Off;
  size: Elf64_Xword;
  link: Elf64_Word;
  info: Elf64_Word;
  addralign: Elf64_Xword;
  entsize: Elf64_Xword;
};

type Elf32_Phdr = struct {
  type: Elf32_Word;
  offset: Elf32_Off;
  vaddr: Elf32_Addr;
  paddr: Elf32_Addr;
  filesz: Elf32_Word;
  memsz: Elf32_Word;
  flags: Elf32_Word;
  align: Elf32_Word;
};

type Elf64_Phdr = struct {
  type: Elf64_Word;
  flags: Elf64_Word;
  offset: Elf64_Off;
  vaddr: Elf64_Addr;
  paddr: Elf64_Addr;
  filesz: Elf64_Xword;
  memsz: Elf64_Xword;
  align: Elf64_Xword;
};

type ElfFile = switch (Elf_Ident.ident.class) {
  1 => struct {
    header: Elf32_Ehdr;
    programHeaders: array[Elf32_Phdr] @ header.phoff with length = header.phnum;
    sectionHeaders: array[Elf32_Shdr] @ header.shoff with length = header.shnum;
  };
  2 => struct {
    header: Elf64_Ehdr;
    programHeaders: array[Elf64_Phdr] @ header.phoff with length = header.phnum;
    sectionHeaders: array[Elf64_Shdr] @ header.shoff with length = header.shnum;
  };
};

root := ElfFile;