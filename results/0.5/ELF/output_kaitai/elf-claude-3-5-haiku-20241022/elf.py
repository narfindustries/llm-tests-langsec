# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        elfclassnone = 0
        elfclass32 = 1
        elfclass64 = 2

    class ElfShType(Enum):
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

    class ElfPType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6

    class ElfVersion(Enum):
        ev_none = 0
        ev_current = 1

    class ElfObjectType(Enum):
        et_none = 0
        et_rel = 1
        et_exec = 2
        et_dyn = 3
        et_core = 4

    class ElfDataEncoding(Enum):
        elfdatanone = 0
        elfdata2lsb = 1
        elfdata2msb = 2

    class ElfOsAbi(Enum):
        elfosabi_sysv = 0
        elfosabi_linux = 3

    class ElfMachine(Enum):
        em_none = 0
        em_sparc = 2
        em_386 = 3
        em_mips = 8
        em_x86_64 = 62
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        _on = self.header.e_ident.ei_data
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
            self.e_type = KaitaiStream.resolve_enum(Elf.ElfObjectType, self._io.read_u2le())
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
            self.e_ident = Elf.IdentStruct(self._io, self, self._root, self._is_le)
            self.e_type = KaitaiStream.resolve_enum(Elf.ElfObjectType, self._io.read_u2be())
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
            self.ei_class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.ei_data = KaitaiStream.resolve_enum(Elf.ElfDataEncoding, self._io.read_u1())
            self.ei_version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u1())
            self.ei_osabi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.ei_abiversion = self._io.read_u1()
            self.ei_pad = self._io.read_bytes(7)

        def _read_be(self):
            self.magic = self._io.read_bytes(4)
            if not self.magic == b"\x7F\x45\x4C\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/types/ident_struct/seq/0")
            self.ei_class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.ei_data = KaitaiStream.resolve_enum(Elf.ElfDataEncoding, self._io.read_u1())
            self.ei_version = KaitaiStream.resolve_enum(Elf.ElfVersion, self._io.read_u1())
            self.ei_osabi = KaitaiStream.resolve_enum(Elf.ElfOsAbi, self._io.read_u1())
            self.ei_abiversion = self._io.read_u1()
            self.ei_pad = self._io.read_bytes(7)


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
            self.p_type = KaitaiStream.resolve_enum(Elf.ElfPType, self._io.read_u4le())
            self.p_flags = self._io.read_u4le()
            self.p_offset = self._io.read_u8le()
            self.p_vaddr = self._io.read_u8le()
            self.p_paddr = self._io.read_u8le()
            self.p_filesz = self._io.read_u8le()
            self.p_memsz = self._io.read_u8le()
            self.p_align = self._io.read_u8le()

        def _read_be(self):
            self.p_type = KaitaiStream.resolve_enum(Elf.ElfPType, self._io.read_u4be())
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
            self.sh_type = KaitaiStream.resolve_enum(Elf.ElfShType, self._io.read_u4le())
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
            self.sh_type = KaitaiStream.resolve_enum(Elf.ElfShType, self._io.read_u4be())
            self.sh_flags = self._io.read_u8be()
            self.sh_addr = self._io.read_u8be()
            self.sh_offset = self._io.read_u8be()
            self.sh_size = self._io.read_u8be()
            self.sh_link = self._io.read_u4be()
            self.sh_info = self._io.read_u4be()
            self.sh_addralign = self._io.read_u8be()
            self.sh_entsize = self._io.read_u8be()



