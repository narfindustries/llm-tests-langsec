# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        elf32 = 1
        elf64 = 2

    class EType(Enum):
        none = 0
        rel = 1
        exec = 2

    class Machine(Enum):
        no_machine = 0
        x86_64 = 62

    class ElfDataEncoding(Enum):
        le = 1
        be = 2

    class ElfOsAbi(Enum):
        system_v = 0
        hp_ux = 1
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
        self.data = KaitaiStream.resolve_enum(Elf.ElfDataEncoding, self._io.read_u1())
        self.version = self._io.read_bytes(1)
        if not self.version == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.version, self._io, u"/seq/3")
        self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        _on = self.class
        if _on == Elf.ElfClass.elf32:
            self.header = Elf.ElfHeader32(self._io, self, self._root)
        elif _on == Elf.ElfClass.elf64:
            self.header = Elf.ElfHeader64(self._io, self, self._root)

    class ElfHeader32(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.EType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u4le()
            self.program_header_offset = self._io.read_u4le()
            self.section_header_offset = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            self.header_size = self._io.read_u2le()
            self.program_header_entry_size = self._io.read_u2le()
            self.program_header_num_entries = self._io.read_u2le()
            self.section_header_entry_size = self._io.read_u2le()
            self.section_header_num_entries = self._io.read_u2le()
            self.section_names_idx = self._io.read_u2le()


    class ElfHeader64(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.EType, self._io.read_u2le())
            self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
            self.version = self._io.read_u4le()
            self.entry_point = self._io.read_u8le()
            self.program_header_offset = self._io.read_u8le()
            self.section_header_offset = self._io.read_u8le()
            self.flags = self._io.read_u4le()
            self.header_size = self._io.read_u2le()
            self.program_header_entry_size = self._io.read_u2le()
            self.program_header_num_entries = self._io.read_u2le()
            self.section_header_entry_size = self._io.read_u2le()
            self.section_header_num_entries = self._io.read_u2le()
            self.section_names_idx = self._io.read_u2le()



