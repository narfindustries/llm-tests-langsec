meta:
  id: elf
  file-extension: elf
  endian: le
  license: GPL-2.0+
doc: |
  The ELF format is a binary format for executables, object code, shared libraries,
  and core dumps. First published in the specification for the application binary
  interface (ABI) of the Unix operating system version named System V Release 4 (SVR4),
  and later in the Tool Interface Standard, it was quickly accepted among different
  vendors of Unix systems. In years since, it has become the most used format for
  binary executables on Unix-like systems on different instruction set architectures,
  including x86, ARM, and others.
seq:
  - id: header
    type: header
  - id: program_headers
    type: program_header
    repeat: expr
    repeat-expr: header.ph_num
  - id: section_headers
    type: section_header
    repeat: expr
    repeat-expr: header.sh_num
types:
  header:
    seq:
      - id: magic
        contents: [0x7F, 0x45, 0x4C, 0x46] # "\x7FELF"
      - id: bits
        type: u1
        enum: bits_type
        doc: "ELF file class (32 or 64-bit)"
      - id: endian
        type: u1
        enum: endian_type
        doc: "Endianess"
      - id: ei_version
        type: u1
        doc: "ELF header version"
      - id: abi
        type: u1
        doc: "Target operating system ABI"
      - id: abi_version
        type: u1
        doc: "ABI version"
      - id: pad
        size: 7
        doc: "Padding bytes"
      - id: type
        type: u2
        enum: obj_type
        doc: "Object file type"
      - id: machine
        type: u2
        enum: machine_type
        doc: "Target instruction set architecture"
      - id: version
        type: u4
        doc: "ELF format version"
      - id: entry_point
        type:
          switch-on: bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
        doc: "Memory address of the entry point from where the process starts executing"
      - id: ph_off
        type:
          switch-on: bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
        doc: "Program header table file offset"
      - id: sh_off
        type:
          switch-on: bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
        doc: "Section header table file offset"
      - id: flags
        type: u4
        doc: "Processor-specific flags"
      - id: eh_size
        type: u2
        doc: "ELF header size"
      - id: ph_entry_size
        type: u2
        doc: "Size of one entry in the file's program header table"
      - id: ph_num
        type: u2
        doc: "Number of entries in the program header table"
      - id: sh_entry_size
        type: u2
        doc: "Size of one entry in the file's section header table"
      - id: sh_num
        type: u2
        doc: "Number of entries in the section header table"
      - id: sh_str_idx
        type: u2
        doc: "Section header string table index"
  program_header:
    seq:
      - id: type
        type: u4
        enum: ph_type
      - id: flags
        type: u4
      - id: offset
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: vaddr
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: paddr
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: filesz
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: memsz
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: align
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
  section_header:
    seq:
      - id: name_offset
        type: u4
      - id: type
        type: u4
        enum: sh_type
      - id: flags
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: addr
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: offset
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: size
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: link
        type: u4
      - id: info
        type: u4
      - id: addr_align
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
      - id: ent_size
        type:
          switch-on: _root.header.bits
          cases:
            'bits_type::b32': u4
            'bits_type::b64': u8
enums:
  bits_type:
    1: b32
    2: b64
  endian_type:
    1: le
    2: be
  obj_type:
    0x00: none
    0x01: rel
    0x02: exec
    0x03: dyn
    0x04: core
    0xFE00: lo_os
    0xFEFF: hi_os
    0xFF00: lo_proc
    0xFFFF: hi_proc
  machine_type:
    0x00: no_machine
    0x02: mips
    0x03: x86
    0x08: mips_rs3_le
    0x14: powerpc
    0x28: arm
    0x2A: superh
    0x32: ia_64
    0x3E: x86_64
    0xB7: aarch64
  ph_type:
    0x0: pt_null
    0x1: pt_load
    0x2: pt_dynamic
    0x3: pt_interp
    0x4: pt_note
    0x5: pt_shlib
    0x6: pt_phdr
    0x7: pt_tls
    0x60000000: pt_lo_os
    0x6FFFFFFF: pt_hi_os
    0x70000000: pt_lo_proc
    0x7FFFFFFF: pt_hi_proc
  sh_type:
    0x0: sht_null
    0x1: sht_progbits
    0x2: sht_symtab
    0x3: sht_strtab
    0x4: sht_rela
    0x5: sht_hash
    0x6: sht_dynamic
    0x7: sht_note
    0x8: sht_nobits
    0x9: sht_rel
    0x0A: sht_shlib
    0x0B: sht_dynsym
    0x0E: sht_init_array
    0x0F: sht_fini_array
    0x10: sht_preinit_array
    0x11: sht_group
    0x12: sht_symtab_shndx
    0x60000000: sht_lo_os
    0x6FFFFFFF: sht_hi_os
    0x70000000: sht_lo_proc
    0x7FFFFFFF: sht_hi_proc