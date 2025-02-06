# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class BitsT(Enum):
        b32 = 1
        b64 = 2

    class EndianT(Enum):
        le = 1
        be = 2

    class OsAbi(Enum):
        system_v = 0
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
        no_machine = 0
        x86 = 3
        mips = 4
        intel_mcu = 7
        sparc = 8
        intel_80860 = 9
        mips_rs3000_le = 10
        parisc = 15
        sparc32plus = 18
        sparc64 = 19
        mips_rs3000 = 20
        arm = 40
        superh = 42
        ia_64 = 50
        x86_64 = 62
        aarch64 = 183
        risc_v = 188
        bpf = 243

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
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = KaitaiStream.resolve_enum(Elf.BitsT, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.EndianT, self._io.read_u1())
        self.ei_version = self._io.read_bytes(1)
        if not self.ei_version == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.ei_version, self._io, u"/seq/3")
        self.abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        _on = self.bits
        if _on == Elf.BitsT.b32:
            self.header = Elf.Header32(self._io, self, self._root)
        elif _on == Elf.BitsT.b64:
            self.header = Elf.Header64(self._io, self, self._root)

    class Header32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.prog_hdr_offset = self._io.read_u4le()
            self.sect_hdr_offset = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.header_size = self._io.read_u2le()
            self.prog_hdr_entry_size = self._io.read_u2le()
            self.prog_hdr_entry_count = self._io.read_u2le()
            self.sect_hdr_entry_size = self._io.read_u2le()
            self.sect_hdr_entry_count = self._io.read_u2le()
            self.sect_names_idx = self._io.read_u2le()


    class Header64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u8le()
            self.prog_hdr_offset = self._io.read_u8le()
            self.sect_hdr_offset = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.header_size = self._io.read_u2le()
            self.prog_hdr_entry_size = self._io.read_u2le()
            self.prog_hdr_entry_count = self._io.read_u2le()
            self.sect_hdr_entry_size = self._io.read_u2le()
            self.sect_hdr_entry_count = self._io.read_u2le()
            self.sect_names_idx = self._io.read_u2le()



