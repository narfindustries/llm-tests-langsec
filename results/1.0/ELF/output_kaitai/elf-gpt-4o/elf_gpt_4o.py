# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class ElfGpt4o(KaitaiStruct):

    class ObjectType(Enum):
        none = 0
        relocatable = 1
        executable = 2
        shared = 3
        core = 4

    class BitsType(Enum):
        bits_32 = 1
        bits_64 = 2

    class OsAbi(Enum):
        system_v = 0
        hpux = 1
        netbsd = 2
        gnu_linux = 3
        gnu_hurd = 4
        solaris = 6
        aix = 7
        irix = 8
        freebsd = 9
        tru64 = 10
        novell_modesto = 11
        openbsd = 12
        openvms = 13
        non_stop_kernel = 14
        ar0s = 15
        fenixos = 16
        cloudabi = 17
        sortix = 53
        arm_abi = 64
        fuchsia = 97
        standalone = 255

    class Endianness(Enum):
        le = 1
        be = 2

    class MachineType(Enum):
        no_machine = 0
        m32 = 1
        sparc = 2
        x86 = 3
        m68k = 4
        m88k = 5
        iamcu = 6
        hppa = 7
        arm = 8
        superh = 9
        ia_64 = 10
        x86_64 = 11
        aarch64 = 12
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = KaitaiStream.resolve_enum(ElfGpt4o.BitsType, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(ElfGpt4o.Endianness, self._io.read_u1())
        self.version = self._io.read_u1()
        self.abi = KaitaiStream.resolve_enum(ElfGpt4o.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        if not self.pad == b"\x00\x00\x00\x00\x00\x00\x00":
            raise kaitaistruct.ValidationNotEqualError(b"\x00\x00\x00\x00\x00\x00\x00", self.pad, self._io, u"/seq/6")
        self.header = ElfGpt4o.HeaderType(self._io, self, self._root)

    class HeaderType(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(ElfGpt4o.ObjectType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(ElfGpt4o.MachineType, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.program_header_offset = self._io.read_u4le()
            self.section_header_offset = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.header_size = self._io.read_u2le()
            self.program_header_size = self._io.read_u2le()
            self.num_program_headers = self._io.read_u2le()
            self.section_header_size = self._io.read_u2le()
            self.num_section_headers = self._io.read_u2le()
            self.section_header_string_table_index = self._io.read_u2le()



