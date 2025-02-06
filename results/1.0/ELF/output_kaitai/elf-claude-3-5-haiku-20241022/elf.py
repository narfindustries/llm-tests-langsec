# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        class_none = 0
        class_32bit = 1
        class_64bit = 2

    class ElfType(Enum):
        type_none = 0
        type_relocatable = 1
        type_executable = 2
        type_shared = 3
        type_core = 4

    class ElfVersion(Enum):
        version_none = 0
        version_current = 1

    class ElfDataEncoding(Enum):
        encoding_none = 0
        encoding_little_endian = 1
        encoding_big_endian = 2

    class ElfOsAbi(Enum):
        abi_system_v = 0
        abi_linux = 3
        abi_solaris = 6
        abi_aix = 7
        abi_irix = 8
        abi_freebsd = 9
        abi_openbsd = 12
        abi_arm = 97

    class ElfMachine(Enum):
        machine_none = 0
        machine_x86 = 3
        machine_arm = 40
        machine_x86_64 = 62
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        _on = self.header.e_ident_struct.data_encoding
        if _on == 1:
            self._is_le = True
        elif _on == 2:
            self._is_le = False
        if not hasattr(self, '_is_le'):
            raise kaitaistruct.UndecidedEndiannessError("/")
        elif self._is_le == True:
            self._read_le()
        elif self._is_le == False:
            self._read_be()

    def _read_le(self):
        self.header = Elf.ElfHeader(self._io, self, self._root, self._is_le)

    def _read_be(self):
        self.header = Elf.ElfHeader(self._io, self, self._root, self._is_le)

    class ElfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/elf_header")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.e_ident_struct = Elf.EIdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2le())
            self.e_machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2le())
            self.e_version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u4le())
            self.e_entry = self._io.read_u8le()
            self.e_phoff = self._io.read_u8le()
            self.e_shoff = self._io.read_u8le()
            self.e_flags = self._io.read_u4le()
            self.e_ehsize = self._io.read_u2le()
            self.e_phentsize = self._io.read_u2le()
            self.e_phnum = self._io.read_u2le()
            self.e_shentsize = self._io.read_u2le()
            self.e_shnum = self._io.read_u2le()
            self.e_shstrndx = self._io.read_u2le()

        def _read_be(self):
            self.e_ident_struct = Elf.EIdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ElfType, self._io.read_u2be())
            self.e_machine = KaitaiStream.resolve_enum(Elf.ElfMachine, self._io.read_u2be())
            self.e_version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u4be())
            self.e_entry = self._io.read_u8be()
            self.e_phoff = self._io.read_u8be()
            self.e_shoff = self._io.read_u8be()
            self.e_flags = self._io.read_u4be()
            self.e_ehsize = self._io.read_u2be()
            self.e_phentsize = self._io.read_u2be()
            self.e_phnum = self._io.read_u2be()
            self.e_shentsize = self._io.read_u2be()
            self.e_shnum = self._io.read_u2be()
            self.e_shstrndx = self._io.read_u2be()


    class EIdentStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/e_ident_struct")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/e_ident_struct/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.ElfDataEncoding, self._io.read_u1())
            self.version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u1())
            self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)

        def _read_be(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/e_ident_struct/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.ElfDataEncoding, self._io.read_u1())
            self.version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u1())
            self.os_abi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)



