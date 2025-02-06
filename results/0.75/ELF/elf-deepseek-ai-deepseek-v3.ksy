meta:
  id: elf
  file-extension: elf
  endian: le
  imports:
    - /common/elf_types
seq:
  - id: e_ident
    type: e_ident
  - id: e_type
    type: u2
    enum: elf_types.elf_et
  - id: e_machine
    type: u2
    enum: elf_types.elf_em
  - id: e_version
    type: u4
    enum: elf_types.elf_ev
  - id: e_entry
    type: u8
    if: _root.is_64
  - id: e_entry_32
    type: u4
    if: not _root.is_64
  - id: e_phoff
    type: u8
    if: _root.is_64
  - id: e_phoff_32
    type: u4
    if: not _root.is_64
  - id: e_shoff
    type: u8
    if: _root.is_64
  - id: e_shoff_32
    type: u4
    if: not _root.is_64
  - id: e_flags
    type: u4
  - id: e_ehsize
    type: u2
  - id: e_phentsize
    type: u2
  - id: e_phnum
    type: u2
  - id: e_shentsize
    type: u2
  - id: e_shnum
    type: u2
  - id: e_shstrndx
    type: u2
types:
  e_ident:
    seq:
      - id: magic
        contents: [0x7f, 'E', 'L', 'F']
      - id: class
        type: u1
        enum: elf_types.elf_class
      - id: data
        type: u1
        enum: elf_types.elf_data
      - id: version
        type: u1
        enum: elf_types.elf_ev
      - id: osabi
        type: u1
        enum: elf_types.elf_osabi
      - id: abiversion
        type: u1
      - id: pad
        size: 7
  ph_entry:
    seq:
      - id: p_type
        type: u4
        enum: elf_types.elf_pt
      - id: p_flags
        type: u4
        if: _root.is_64
      - id: p_offset
        type: u8
        if: _root.is_64
      - id: p_offset_32
        type: u4
        if: not _root.is_64
      - id: p_vaddr
        type: u8
        if: _root.is_64
      - id: p_vaddr_32
        type: u4
        if: not _root.is_64
      - id: p_paddr
        type: u8
        if: _root.is_64
      - id: p_paddr_32
        type: u4
        if: not _root.is_64
      - id: p_filesz
        type: u8
        if: _root.is_64
      - id: p_filesz_32
        type: u4
        if: not _root.is_64
      - id: p_memsz
        type: u8
        if: _root.is_64
      - id: p_memsz_32
        type: u4
        if: not _极root.is_64
      - id: p_align
        type: u8
        if: _root.is_64
      - id: p_align_32
        type: u4
        if: not _root.is_64
  sh_entry:
    seq:
      - id: sh_name
        type: u4
     极 - id: sh_type
        type: u4
        enum: elf_types.elf_sh
      - id: sh_flags
        type: u8
        if: _root.is_64
      - id: sh_flags_32
        type: u4
        if: not _root.is_64
      - id: sh_addr
        type: u8
        if: _root.is_64
      - id: sh_addr_32
        type: u4
        if: not _root.is_64
      - id: sh_offset
        type: u8
        if: _root.is_64
      - id: sh_offset_32
        type: u4
        if: not _root.is_64
      - id: sh_size
        type: u8
        if: _root.is_64
      - id: sh_size_32
        type: u4
        if: not _root.is_64
      - id: sh_link
        type: u4
      - id: sh_info
        type: u4
      - id: sh_addralign
        type: u8
        if: _root.is_64
      - id: sh_addralign_32
        type: u4
        if: not _root.is_64
      - id: sh_entsize
        type: u8
        if: _root.is_64
      - id: sh_entsize_32
        type: u4
        if: not _root.is_64
instances:
  is_64:
    value: e_ident.class == elf_types.elf_class.elfclass64
  is_le:
    value: e_ident.data == elf_types.elf_data.elfdata2lsb
  is_be:
    value: e_ident.data == elf_types.elf_data.elfdata2msb
  ph_entries:
    pos: e_phoff
    size: e_phentsize * e_phnum
    type: ph_entry
    repeat: expr
    repeat-expr: e_phnum
    if: _root.is_64 and e_phoff != 0
  ph_entries_32:
    pos: e_phoff_32
    size: e_phentsize * e_phnum
    type: ph_entry
    repeat: expr
    repeat-expr: e_phnum
    if: not _root.is_64 and e_phoff_32 != 0
  sh_entries:
    pos: e_shoff
    size: e_shentsize * e_shnum
    type: sh_entry
    repeat: expr
    repeat-expr: e_shnum
    if: _root.is_64 and e_shoff != 0
  sh_entries_32:
    pos: e_shoff_32
    size: e_shentsize * e_shnum
    type: sh_entry
    repeat: expr
    repeat-expr: e_shnum
    if: not _root.is_64 and e_shoff_32 != 0