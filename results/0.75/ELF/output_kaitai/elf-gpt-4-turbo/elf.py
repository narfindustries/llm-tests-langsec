# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    """The ELF file format is used for executables, shared libraries, and core dumps. This spec covers the ELF 64-bit structures.
    """

    class EndianType(Enum):
        le = 1
        be = 2

    class BitsType(Enum):
        b32 = 1
        b64 = 2

    class OsAbi(Enum):
        system_v = 0
        hpux = 1
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

    class MachineType(Enum):
        none = 0
        x86 = 3
        x86_64 = 62

    class ObjType(Enum):
        none = 0
        rel = 1
        exec = 2
        dyn = 3
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
        self.bits = KaitaiStream.resolve_enum(Elf.BitsType, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.EndianType, self._io.read_u1())
        self.header_version = self._io.read_bytes(1)
        if not self.header_version == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.header_version, self._io, u"/seq/3")
        self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.MachineType, self._io.read_u2le())
        self.version = self._io.read_u4le()
        self.entry_point = self._io.read_u8le()
        self.ph_off = self._io.read_u8le()
        self.sh_off = self._io.read_u8le()
        self.flags = self._io.read_u4le()
        self.eh_size = self._io.read_u2le()
        self.ph_ent_size = self._io.read_u2le()
        self.ph_num = self._io.read_u2le()
        self.sh_ent_size = self._io.read_u2le()
        self.sh_num = self._io.read_u2le()
        self.sh_str_idx = self._io.read_u2le()


