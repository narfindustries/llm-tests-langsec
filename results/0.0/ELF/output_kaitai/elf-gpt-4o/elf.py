# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        elfclass32 = 1
        elfclass64 = 2

    class SectionType(Enum):
        sht_null = 0
        sht_progbits = 1
        sht_symtab = 2
        sht_strtab = 3
        sht_rela = 4
        sht_hash = 5
        sht_dynamic = 6
        sht_note = 7
        sht_nobits = 8
        sht_rel = 9
        sht_shlib = 10
        sht_dynsym = 11

    class ElfData(Enum):
        elfdata2lsb = 1
        elfdata2msb = 2

    class ElfOsabi(Enum):
        elfosabi_sysv = 0
        elfosabi_linux = 3

    class SegmentType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6
        pt_tls = 7
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Elf.ElfHeader(self._io, self, self._root)

    class ElfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.e_ident = Elf.EIdent(self._io, self, self._root)
            self.e_type = self._io.read_u2be()
            self.e_machine = self._io.read_u2be()
            self.e_version = self._io.read_u4be()
            self.e_entry = self._io.read_u4be()
            self.e_phoff = self._io.read_u4be()
            self.e_shoff = self._io.read_u4be()
            self.e_flags = self._io.read_u4be()
            self.e_ehsize = self._io.read_u2be()
            self.e_phentsize = self._io.read_u2be()
            self.e_phnum = self._io.read_u2be()
            self.e_shentsize = self._io.read_u2be()
            self.e_shnum = self._io.read_u2be()
            self.e_shstrndx = self._io.read_u2be()


    class EIdent(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/e_ident/seq/0")
            self.class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
            self.version = self._io.read_u1()
            self.osabi = KaitaiStream.resolve_enum(Elf.ElfOsabi, self._io.read_u1())
            self.abiversion = self._io.read_u1()
            self.pad = self._io.read_bytes(7)


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.p_type = KaitaiStream.resolve_enum(Elf.SegmentType, self._io.read_u4be())
            self.p_offset = self._io.read_u4be()
            self.p_vaddr = self._io.read_u4be()
            self.p_paddr = self._io.read_u4be()
            self.p_filesz = self._io.read_u4be()
            self.p_memsz = self._io.read_u4be()
            self.p_flags = self._io.read_u4be()
            self.p_align = self._io.read_u4be()


    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sh_name = self._io.read_u4be()
            self.sh_type = KaitaiStream.resolve_enum(Elf.SectionType, self._io.read_u4be())
            self.sh_flags = self._io.read_u4be()
            self.sh_addr = self._io.read_u4be()
            self.sh_offset = self._io.read_u4be()
            self.sh_size = self._io.read_u4be()
            self.sh_link = self._io.read_u4be()
            self.sh_info = self._io.read_u4be()
            self.sh_addralign = self._io.read_u4be()
            self.sh_entsize = self._io.read_u4be()



