meta:
  id: elf_gpt_4o
  title: ELF File Format
  file-extension: elf
  xref:
    wikidata: Q183168
  license: CC0-1.0
  endian: le

seq:
  - id: magic
    contents: [0x7f, 0x45, 0x4c, 0x46]

  - id: bits
    type: u1
    enum: bits_type

  - id: endian
    type: u1
    enum: endianness

  - id: version
    type: u1

  - id: abi
    type: u1
    enum: os_abi

  - id: abi_version
    type: u1

  - id: pad
    size: 7
    contents: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

  - id: header
    type: header_type

types:
  header_type:
    seq:
      - id: type
        type: u2
        enum: object_type

      - id: machine
        type: u2
        enum: machine_type

      - id: version
        type: u4

      - id: entry_point
        type: u4

      - id: program_header_offset
        type: u4

      - id: section_header_offset
        type: u4

      - id: flags
        type: u4

      - id: header_size
        type: u2

      - id: program_header_size
        type: u2

      - id: num_program_headers
        type: u2

      - id: section_header_size
        type: u2

      - id: num_section_headers
        type: u2

      - id: section_header_string_table_index
        type: u2

enums:
  bits_type:
    1: bits_32
    2: bits_64

  endianness:
    1: le
    2: be

  os_abi:
    0: system_v
    1: hpux
    2: netbsd
    3: gnu_linux
    4: gnu_hurd
    6: solaris
    7: aix
    8: irix
    9: freebsd
    10: tru64
    11: novell_modesto
    12: openbsd
    13: openvms
    14: non_stop_kernel
    15: ar0s
    16: fenixos
    17: cloudabi
    53: sortix
    64: arm_abi
    97: fuchsia
    255: standalone

  object_type:
    0: none
    1: relocatable
    2: executable
    3: shared
    4: core

  machine_type:
    0: no_machine
    1: m32
    2: sparc
    3: x86
    4: m68k
    5: m88k
    6: iamcu
    7: hppa
    8: arm
    9: superh
    10: ia_64
    11: x86_64
    12: aarch64