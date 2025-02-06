namespace ELF;

struct Elf_Ident {
    u32 magic; // 0x7F 'E' 'L' 'F'
    u8  class;
    u8  data;
    u8  version;
    u8  osabi;
    u8  abiversion;
    u8[7] pad;
}

struct Elf32_Header {
    Elf_Ident ident;
    u16 type;
    u16 machine;
    u32 version;
    u32 entry;
    u32 phoff;
    u32 shoff;
    u32 flags;
    u16 ehsize;
    u16 phentsize;
    u16 phnum;
    u16 shentsize;
    u16 shnum;
    u16 shstrndx;
}

struct Elf64_Header {
    Elf_Ident ident;
    u16 type;
    u16 machine;
    u32 version;
    u64 entry;
    u64 phoff;
    u64 shoff;
    u32 flags;
    u16 ehsize;
    u16 phentsize;
    u16 phnum;
    u16 shentsize;
    u16 shnum;
    u16 shstrndx;
}

struct Elf32_Shdr {
    u32 name;
    u32 type;
    u32 flags;
    u32 addr;
    u32 offset;
    u32 size;
    u32 link;
    u32 info;
    u32 addralign;
    u32 entsize;
}

struct Elf64_Shdr {
    u32 name;
    u32 type;
    u64 flags;
    u64 addr;
    u64 offset;
    u64 size;
    u32 link;
    u32 info;
    u64 addralign;
    u64 entsize;
}

struct Elf32_Phdr {
    u32 type;
    u32 offset;
    u32 vaddr;
    u32 paddr;
    u32 filesz;
    u32 memsz;
    u32 flags;
    u32 align;
}

struct Elf64_Phdr {
    u32 type;
    u32 flags;
    u64 offset;
    u64 vaddr;
    u64 paddr;
    u64 filesz;
    u64 memsz;
    u64 align;
}

variant Elf_Header {
    Elf32_Header if ident.class == 1;
    Elf64_Header if ident.class == 2;
}

variant Elf_Shdr {
    Elf32_Shdr if ident.class == 1;
    Elf64_Shdr if ident.class == 2;
}

variant Elf_Phdr {
    Elf32_Phdr if ident.class == 1;
    Elf64_Phdr if ident.class == 2;
}

struct Elf_File {
    Elf_Header header;
    Elf_Phdr[header.phnum] programHeaders @ header.phoff;
    Elf_Shdr[header.shnum] sectionHeaders @ header.shoff;
}