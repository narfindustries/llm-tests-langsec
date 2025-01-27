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
        openvms = 13
        nsk = 14
        aros = 15

    class Machine(Enum):
        none = 0
        sparc = 2
        x86 = 3
        mips = 8
        powerpc = 20
        arm = 40
        superh = 42
        ia64 = 50
        x86_64 = 62
        aarch64 = 183

    class Bits(Enum):
        b32 = 1
        b64 = 2

    class Type(Enum):
        none = 0
        relocatable = 1
        executable = 2
        shared = 3
        core = 4
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
        self.version = self._io.read_u1()
        self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.Type, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.version_2 = self._io.read_u4le()
        self.entry_point = self._io.read_u4le()
        self.program_header_offset = self._io.read_u4le()
        self.section_header_offset = self._io.read_u4le()
        self.flags = self._io.read_u4le()
        self.header_size = self._io.read_u2le()
        self.program_header_entry_size = self._io.read_u2le()
        self.program_header_count = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.section_header_count = self._io.read_u2le()
        self.section_names_idx = self._io.read_u2le()

    @property
    def program_headers(self):
        if hasattr(self, '_m_program_headers'):
            return self._m_program_headers

        _pos = self._io.pos()
        self._io.seek(self.program_header_offset)
        self._m_program_headers = []
        for i in range(self.program_header_count):
            self._m_program_headers.append(self._io.read_bytes(self.program_header_entry_size))

        self._io.seek(_pos)
        return getattr(self, '_m_program_headers', None)

    @property
    def section_headers(self):
        if hasattr(self, '_m_section_headers'):
            return self._m_section_headers

        _pos = self._io.pos()
        self._io.seek(self.section_header_offset)
        self._m_section_headers = []
        for i in range(self.section_header_count):
            self._m_section_headers.append(self._io.read_bytes(self.section_header_entry_size))

        self._io.seek(_pos)
        return getattr(self, '_m_section_headers', None)


