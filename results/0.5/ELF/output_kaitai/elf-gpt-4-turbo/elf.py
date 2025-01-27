# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    """The ELF format is a binary format for executables, object code, shared libraries,
    and core dumps. First published in the specification for the application binary
    interface (ABI) of the Unix operating system version named System V Release 4 (SVR4),
    and later in the Tool Interface Standard, it was quickly accepted among different
    vendors of Unix systems. In years since, it has become the most used format for
    binary executables on Unix-like systems on different instruction set architectures,
    including x86, ARM, and others.
    """

    class EndianType(Enum):
        le = 1
        be = 2

    class BitsType(Enum):
        b32 = 1
        b64 = 2

    class ShType(Enum):
        sht_null = 0
        sht_progbits = 1
        sht_symtab = 2
        sht_strtab = 3
        sht_rela = 4
        sht_hash = 5
        sht_dynamic = 6
        sht_note = 7
        sht_nobits = 8
        sht_rel = 9
        sht_shlib = 10
        sht_dynsym = 11
        sht_init_array = 14
        sht_fini_array = 15
        sht_preinit_array = 16
        sht_group = 17
        sht_symtab_shndx = 18
        sht_lo_os = 1610612736
        sht_hi_os = 1879048191
        sht_lo_proc = 1879048192
        sht_hi_proc = 2147483647

    class MachineType(Enum):
        no_machine = 0
        mips = 2
        x86 = 3
        mips_rs3_le = 8
        powerpc = 20
        arm = 40
        superh = 42
        ia_64 = 50
        x86_64 = 62
        aarch64 = 183

    class PhType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6
        pt_tls = 7
        pt_lo_os = 1610612736
        pt_hi_os = 1879048191
        pt_lo_proc = 1879048192
        pt_hi_proc = 2147483647

    class ObjType(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
        core = 4
        lo_os = 65024
        hi_os = 65279
        lo_proc = 65280
        hi_proc = 65535
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Elf.Header(self._io, self, self._root)
        self.program_headers = []
        for i in range(self.header.ph_num):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

        self.section_headers = []
        for i in range(self.header.sh_num):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root))


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/header/seq/0")
            self.bits = KaitaiStream.resolve_enum(Elf.BitsType, self._io.read_u1())
            self.endian = KaitaiStream.resolve_enum(Elf.EndianType, self._io.read_u1())
            self.ei_version = self._io.read_u1()
            self.abi = self._io.read_u1()
            self.abi_version = self._io.read_u1()
            self.pad = self._io.read_bytes(7)
            self.type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.MachineType, self._io.read_u2le())
            self.version = self._io.read_u4le()
            _on = self.bits
            if _on == Elf.BitsType.b32:
                self.entry_point = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.entry_point = self._io.read_u8le()
            _on = self.bits
            if _on == Elf.BitsType.b32:
                self.ph_off = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.ph_off = self._io.read_u8le()
            _on = self.bits
            if _on == Elf.BitsType.b32:
                self.sh_off = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.sh_off = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.eh_size = self._io.read_u2le()
            self.ph_entry_size = self._io.read_u2le()
            self.ph_num = self._io.read_u2le()
            self.sh_entry_size = self._io.read_u2le()
            self.sh_num = self._io.read_u2le()
            self.sh_str_idx = self._io.read_u2le()


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            self.flags = self._io.read_u4le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.vaddr = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.vaddr = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.paddr = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.paddr = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.filesz = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.filesz = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.memsz = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.memsz = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.align = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.align = self._io.read_u8le()


    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = KaitaiStream.resolve_enum(Elf.ShType, self._io.read_u4le())
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.flags = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.flags = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.addr = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.addr = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.size = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.size = self._io.read_u8le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.addr_align = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.addr_align = self._io.read_u8le()
            _on = self._root.header.bits
            if _on == Elf.BitsType.b32:
                self.ent_size = self._io.read_u4le()
            elif _on == Elf.BitsType.b64:
                self.ent_size = self._io.read_u8le()



