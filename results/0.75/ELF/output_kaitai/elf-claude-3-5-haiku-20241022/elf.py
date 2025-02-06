# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        class_32 = 1
        class_64 = 2

    class ArchType(Enum):
        sparc = 2
        x86 = 3
        mips = 8
        powerpc = 20
        arm = 40
        ia64 = 50
        x86_64 = 62
        aarch64 = 183

    class DataEncodingType(Enum):
        little_endian = 1
        big_endian = 2

    class OsAbiType(Enum):
        system_v = 0
        hp_ux = 1
        netbsd = 2
        linux = 3
        solaris = 6
        freebsd = 9
        openbsd = 12
        standalone = 255

    class ObjType(Enum):
        et_none = 0
        et_rel = 1
        et_exec = 2
        et_dyn = 3
        et_core = 4
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        _on = self.header.e_ident.class
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
            self.e_ident = Elf.IdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
            self.e_machine = KaitaiStream.resolve_enum(Elf.ArchType, self._io.read_u2le())
            self.e_version = self._io.read_u4le()
            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_entry = self._io.read_u8le()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_entry_32 = self._io.read_u4le()

            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_phoff = self._io.read_u8le()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_phoff_32 = self._io.read_u4le()

            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_shoff = self._io.read_u8le()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_shoff_32 = self._io.read_u4le()

            self.e_flags = self._io.read_u4le()
            self.e_ehsize = self._io.read_u2le()
            self.e_phentsize = self._io.read_u2le()
            self.e_phnum = self._io.read_u2le()
            self.e_shentsize = self._io.read_u2le()
            self.e_shnum = self._io.read_u2le()
            self.e_shstrndx = self._io.read_u2le()

        def _read_be(self):
            self.e_ident = Elf.IdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2be())
            self.e_machine = KaitaiStream.resolve_enum(Elf.ArchType, self._io.read_u2be())
            self.e_version = self._io.read_u4be()
            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_entry = self._io.read_u8be()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_entry_32 = self._io.read_u4be()

            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_phoff = self._io.read_u8be()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_phoff_32 = self._io.read_u4be()

            if self.e_ident.class == Elf.ElfClass.class_64:
                self.e_shoff = self._io.read_u8be()

            if self.e_ident.class == Elf.ElfClass.class_32:
                self.e_shoff_32 = self._io.read_u4be()

            self.e_flags = self._io.read_u4be()
            self.e_ehsize = self._io.read_u2be()
            self.e_phentsize = self._io.read_u2be()
            self.e_phnum = self._io.read_u2be()
            self.e_shentsize = self._io.read_u2be()
            self.e_shnum = self._io.read_u2be()
            self.e_shstrndx = self._io.read_u2be()


    class IdentStruct(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/ident_struct")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/ident_struct/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.DataEncodingType, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbiType, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)

        def _read_be(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/ident_struct/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.DataEncodingType, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbiType, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)



