# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    """The Executable and Linkable Format (ELF) is a common standard file format for
    executables, object code, shared libraries, and core dumps. First published
    in the specification for the application binary interface (ABI) of the Unix
    operating system version named System V Release 4 (SVR4), and later in the
    Tool Interface Standard, it was quickly accepted among different vendors of
    Unix systems. In years since, it has become one of the most used formats for
    binary executables in Unix and Unix-like systems on many hardware platforms.
    """

    class MachineType(Enum):
        sparc = 2
        x86 = 3
        mips = 8
        powerpc = 20
        arm = 40
        x86_64 = 62
        aarch64 = 183
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = self._io.read_u1()
        self.endian = self._io.read_u1()
        self.ei_version = self._io.read_u1()
        self.abi = self._io.read_u1()
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        _on = self.bits
        if _on == 1:
            self.header = Elf.Header32(self._io, self, self._root)
        elif _on == 2:
            self.header = Elf.Header64(self._io, self, self._root)

    class Header32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u2le()
            self.machine = self._io.read_u2le()
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.ph_off = self._io.read_u4le()
            self.sh_off = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.eh_size = self._io.read_u2le()
            self.ph_entry_size = self._io.read_u2le()
            self.ph_num = self._io.read_u2le()
            self.sh_entry_size = self._io.read_u2le()
            self.sh_num = self._io.read_u2le()
            self.sh_str_idx = self._io.read_u2le()


    class Header64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u2le()
            self.machine = self._io.read_u2le()
            self.version = self._io.read_u8le()
            self.entry_point = self._io.read_u8le()
            self.ph_off = self._io.read_u8le()
            self.sh_off = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.eh_size = self._io.read_u2le()
            self.ph_entry_size = self._io.read_u2le()
            self.ph_num = self._io.read_u2le()
            self.sh_entry_size = self._io.read_u2le()
            self.sh_num = self._io.read_u2le()
            self.sh_str_idx = self._io.read_u2le()



