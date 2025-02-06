# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class Endian(Enum):
        le = 1
        be = 2

    class ObjectType(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
        core = 4
        loos = 65024
        hios = 65279
        loproc = 65280
        hiproc = 65535

    class ShType(Enum):
        null_type = 0
        progbits = 1
        symtab = 2
        strtab = 3
        rela = 4
        hash = 5
        dynamic = 6
        note = 7
        nobits = 8
        rel = 9
        shlib = 10
        dynsym = 11

    class OsAbi(Enum):
        system_v = 0
        hp_ux = 1
        netbsd = 2
        linux = 3
        solaris = 6
        aix = 7
        irix = 8
        freebsd = 9
        tru64 = 10
        modesto = 11
        openbsd = 12
        arm_aeabi = 64
        arm = 97
        standalone = 255

    class Machine(Enum):
        none = 0
        m32 = 1
        sparc = 2
        i386 = 3
        m68k = 4
        m88k = 5
        i860 = 7
        mips = 8
        x86_64 = 62

    class Bits(Enum):
        b32 = 1
        b64 = 2

    class PhType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6
        pt_tls = 7
        pt_loos = 1610612736
        pt_hios = 1879048191
        pt_loproc = 1879048192
        pt_hiproc = 2147483647
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = KaitaiStream.resolve_enum(Elf.Bits, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.Endian, self._io.read_u1())
        self.ei_version = self._io.read_u1()
        self.abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.padding = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.ObjectType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.version = self._io.read_u4le()
        if self.bits == Elf.Bits.b32:
            self.entry_point = self._io.read_u4le()

        if self.bits == Elf.Bits.b64:
            self.entry_point_64 = self._io.read_u8le()

        if self.bits == Elf.Bits.b32:
            self.program_header_offset = self._io.read_u4le()

        if self.bits == Elf.Bits.b64:
            self.program_header_offset_64 = self._io.read_u8le()

        if self.bits == Elf.Bits.b32:
            self.section_header_offset = self._io.read_u4le()

        if self.bits == Elf.Bits.b64:
            self.section_header_offset_64 = self._io.read_u8le()

        self.flags = self._io.read_u4le()
        self.header_size = self._io.read_u2le()
        self.program_header_entry_size = self._io.read_u2le()
        self.program_header_num_entries = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.section_header_num_entries = self._io.read_u2le()
        self.section_names_idx = self._io.read_u2le()

    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            self.offset = self._io.read_u4le()
            self.vaddr = self._io.read_u4le()
            self.paddr = self._io.read_u4le()
            self.filesz = self._io.read_u4le()
            self.memsz = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.align = self._io.read_u4le()


    class ProgramHeader64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            self.flags = self._io.read_u4le()
            self.offset = self._io.read_u8le()
            self.vaddr = self._io.read_u8le()
            self.paddr = self._io.read_u8le()
            self.filesz = self._io.read_u8le()
            self.memsz = self._io.read_u8le()
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
            self.flags = self._io.read_u4le()
            self.addr = self._io.read_u4le()
            self.offset = self._io.read_u4le()
            self.size = self._io.read_u4le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            self.align = self._io.read_u4le()
            self.entry_size = self._io.read_u4le()


    class SectionHeader64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = KaitaiStream.resolve_enum(Elf.ShType, self._io.read_u4le())
            self.flags = self._io.read_u8le()
            self.addr = self._io.read_u8le()
            self.offset = self._io.read_u8le()
            self.size = self._io.read_u8le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            self.align = self._io.read_u8le()
            self.entry_size = self._io.read_u8le()


    @property
    def program_headers(self):
        if hasattr(self, '_m_program_headers'):
            return self._m_program_headers

        if self.bits == Elf.Bits.b32:
            _pos = self._io.pos()
            self._io.seek(self.program_header_offset)
            self._m_program_headers = []
            for i in range(self.program_header_num_entries):
                self._m_program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

            self._io.seek(_pos)

        return getattr(self, '_m_program_headers', None)

    @property
    def program_headers_64(self):
        if hasattr(self, '_m_program_headers_64'):
            return self._m_program_headers_64

        if self.bits == Elf.Bits.b64:
            _pos = self._io.pos()
            self._io.seek(self.program_header_offset_64)
            self._m_program_headers_64 = []
            for i in range(self.program_header_num_entries):
                self._m_program_headers_64.append(Elf.ProgramHeader64(self._io, self, self._root))

            self._io.seek(_pos)

        return getattr(self, '_m_program_headers_64', None)

    @property
    def section_headers(self):
        if hasattr(self, '_m_section_headers'):
            return self._m_section_headers

        if self.bits == Elf.Bits.b32:
            _pos = self._io.pos()
            self._io.seek(self.section_header_offset)
            self._m_section_headers = []
            for i in range(self.section_header_num_entries):
                self._m_section_headers.append(Elf.SectionHeader(self._io, self, self._root))

            self._io.seek(_pos)

        return getattr(self, '_m_section_headers', None)

    @property
    def section_headers_64(self):
        if hasattr(self, '_m_section_headers_64'):
            return self._m_section_headers_64

        if self.bits == Elf.Bits.b64:
            _pos = self._io.pos()
            self._io.seek(self.section_header_offset_64)
            self._m_section_headers_64 = []
            for i in range(self.section_header_num_entries):
                self._m_section_headers_64.append(Elf.SectionHeader64(self._io, self, self._root))

            self._io.seek(_pos)

        return getattr(self, '_m_section_headers_64', None)


