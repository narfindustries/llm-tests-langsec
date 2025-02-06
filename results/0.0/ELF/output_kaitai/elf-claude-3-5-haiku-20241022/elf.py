# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ObjectFileType(Enum):
        none = 0
        relocatable = 1
        executable = 2
        shared_object = 3
        core = 4

    class MachineArchitecture(Enum):
        sparc = 2
        x86 = 3
        mips = 8
        powerpc = 20
        arm = 40
        x86_64 = 62

    class OsAbi(Enum):
        system_v = 0
        hp_ux = 1
        netbsd = 2
        linux = 3
        solaris = 6
        freebsd = 9
        arm = 12

    class DataEncoding(Enum):
        none = 0
        little_endian = 1
        big_endian = 2

    class FileClass(Enum):
        none = 0
        class32 = 1
        class64 = 2
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        _on = self.header.e_ident.data_encoding
        if _on == Elf.DataEncoding.little_endian:
            self._is_le = True
        elif _on == Elf.DataEncoding.big_endian:
            self._is_le = False
        if not hasattr(self, '_is_le'):
            raise kaitaistruct.UndecidedEndiannessError("/")
        elif self._is_le == True:
            self._read_le()
        elif self._is_le == False:
            self._read_be()

    def _read_le(self):
        self.header = Elf.ElfHeader(self._io, self, self._root, self._is_le)
        self.program_headers = []
        for i in range(self.header.e_phnum):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root, self._is_le))

        self.section_headers = []
        for i in range(self.header.e_shnum):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root, self._is_le))


    def _read_be(self):
        self.header = Elf.ElfHeader(self._io, self, self._root, self._is_le)
        self.program_headers = []
        for i in range(self.header.e_phnum):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root, self._is_le))

        self.section_headers = []
        for i in range(self.header.e_shnum):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root, self._is_le))


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
            self.file_class = KaitaiStream.resolve_enum(Elf.FileClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.DataEncoding, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)

        def _read_be(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/ident_struct/seq/0")
            self.file_class = KaitaiStream.resolve_enum(Elf.FileClass, self._io.read_u1())
            self.data_encoding = KaitaiStream.resolve_enum(Elf.DataEncoding, self._io.read_u1())
            self.version = self._io.read_u1()
            self.os_abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
            self.abi_version = self._io.read_u1()
            self.padding = self._io.read_bytes(7)


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
            self.e_type = KaitaiStream.resolve_enum(Elf.ObjectFileType, self._io.read_u2le())
            self.e_machine = KaitaiStream.resolve_enum(Elf.MachineArchitecture, self._io.read_u2le())
            self.e_version = self._io.read_u4le()
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
            self.e_ident = Elf.IdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ObjectFileType, self._io.read_u2be())
            self.e_machine = KaitaiStream.resolve_enum(Elf.MachineArchitecture, self._io.read_u2be())
            self.e_version = self._io.read_u4be()
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


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/program_header")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.p_type = self._io.read_u4le()
            self.p_flags = self._io.read_u4le()
            self.p_offset = self._io.read_u8le()
            self.p_vaddr = self._io.read_u8le()
            self.p_paddr = self._io.read_u8le()
            self.p_filesz = self._io.read_u8le()
            self.p_memsz = self._io.read_u8le()
            self.p_align = self._io.read_u8le()

        def _read_be(self):
            self.p_type = self._io.read_u4be()
            self.p_flags = self._io.read_u4be()
            self.p_offset = self._io.read_u8be()
            self.p_vaddr = self._io.read_u8be()
            self.p_paddr = self._io.read_u8be()
            self.p_filesz = self._io.read_u8be()
            self.p_memsz = self._io.read_u8be()
            self.p_align = self._io.read_u8be()


    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None, _is_le=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._is_le = _is_le
            self._read()

        def _read(self):
            if not hasattr(self, '_is_le'):
                raise kaitaistruct.UndecidedEndiannessError("/types/section_header")
            elif self._is_le == True:
                self._read_le()
            elif self._is_le == False:
                self._read_be()

        def _read_le(self):
            self.sh_name = self._io.read_u4le()
            self.sh_type = self._io.read_u4le()
            self.sh_flags = self._io.read_u8le()
            self.sh_addr = self._io.read_u8le()
            self.sh_offset = self._io.read_u8le()
            self.sh_size = self._io.read_u8le()
            self.sh_link = self._io.read_u4le()
            self.sh_info = self._io.read_u4le()
            self.sh_addralign = self._io.read_u8le()
            self.sh_entsize = self._io.read_u8le()

        def _read_be(self):
            self.sh_name = self._io.read_u4be()
            self.sh_type = self._io.read_u4be()
            self.sh_flags = self._io.read_u8be()
            self.sh_addr = self._io.read_u8be()
            self.sh_offset = self._io.read_u8be()
            self.sh_size = self._io.read_u8be()
            self.sh_link = self._io.read_u4be()
            self.sh_info = self._io.read_u4be()
            self.sh_addralign = self._io.read_u8be()
            self.sh_entsize = self._io.read_u8be()



