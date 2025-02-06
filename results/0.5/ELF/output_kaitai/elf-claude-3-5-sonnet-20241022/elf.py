# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class Endian(Enum):
        le = 1
        be = 2

    class EType(Enum):
        et_none = 0
        et_rel = 1
        et_exec = 2
        et_dyn = 3
        et_core = 4
        et_loos = 65024
        et_hios = 65279
        et_loproc = 65280
        et_hiproc = 65535

    class ShType(Enum):
        sh_null = 0
        sh_progbits = 1
        sh_symtab = 2
        sh_strtab = 3
        sh_rela = 4
        sh_hash = 5
        sh_dynamic = 6
        sh_note = 7
        sh_nobits = 8
        sh_rel = 9
        sh_shlib = 10
        sh_dynsym = 11

    class OsAbi(Enum):
        system_v = 0
        hp_ux = 1
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

    class Machine(Enum):
        em_none = 0
        em_m32 = 1
        em_sparc = 2
        em_386 = 3
        em_68k = 4
        em_88k = 5
        em_860 = 7
        em_mips = 8
        em_x86_64 = 62

    class Class(Enum):
        class_32 = 1
        class_64 = 2

    class PhType(Enum):
        pt_null = 0
        pt_load = 1
        pt_dynamic = 2
        pt_interp = 3
        pt_note = 4
        pt_shlib = 5
        pt_phdr = 6
        pt_tls = 7
        pt_loos = 1610612736
        pt_hios = 1879048191
        pt_loproc = 1879048192
        pt_hiproc = 2147483647
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.ei_class = KaitaiStream.resolve_enum(Elf.Class, self._io.read_u1())
        self.ei_data = KaitaiStream.resolve_enum(Elf.Endian, self._io.read_u1())
        self.ei_version = self._io.read_u1()
        self.ei_osabi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.ei_abiversion = self._io.read_u1()
        self.ei_pad = self._io.read_bytes(7)
        self.e_type = KaitaiStream.resolve_enum(Elf.EType, self._io.read_u2le())
        self.e_machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.e_version = self._io.read_u4le()
        if self.ei_class == Elf.Class.class_32:
            self.e_entry = self._io.read_u4le()

        if self.ei_class == Elf.Class.class_64:
            self.e_entry_64 = self._io.read_u8le()

        if self.ei_class == Elf.Class.class_32:
            self.e_phoff = self._io.read_u4le()

        if self.ei_class == Elf.Class.class_64:
            self.e_phoff_64 = self._io.read_u8le()

        if self.ei_class == Elf.Class.class_32:
            self.e_shoff = self._io.read_u4le()

        if self.ei_class == Elf.Class.class_64:
            self.e_shoff_64 = self._io.read_u8le()

        self.e_flags = self._io.read_u4le()
        self.e_ehsize = self._io.read_u2le()
        self.e_phentsize = self._io.read_u2le()
        self.e_phnum = self._io.read_u2le()
        self.e_shentsize = self._io.read_u2le()
        self.e_shnum = self._io.read_u2le()
        self.e_shstrndx = self._io.read_u2le()
        self.program_headers = []
        for i in range(self.e_phnum):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

        self.section_headers = []
        for i in range(self.e_shnum):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root))


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.p_type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            if self._root.ei_class == Elf.Class.class_64:
                self.p_flags = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_offset = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_offset_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_vaddr = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_vaddr_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_paddr = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_paddr_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_filesz = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_filesz_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_memsz = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_memsz_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_flags_32 = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_32:
                self.p_align = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.p_align_64 = self._io.read_u8le()



    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sh_name = self._io.read_u4le()
            self.sh_type = KaitaiStream.resolve_enum(Elf.ShType, self._io.read_u4le())
            if self._root.ei_class == Elf.Class.class_32:
                self.sh_flags = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_flags_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.sh_addr = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_addr_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.sh_offset = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_offset_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.sh_size = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_size_64 = self._io.read_u8le()

            self.sh_link = self._io.read_u4le()
            self.sh_info = self._io.read_u4le()
            if self._root.ei_class == Elf.Class.class_32:
                self.sh_addralign = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_addralign_64 = self._io.read_u8le()

            if self._root.ei_class == Elf.Class.class_32:
                self.sh_entsize = self._io.read_u4le()

            if self._root.ei_class == Elf.Class.class_64:
                self.sh_entsize_64 = self._io.read_u8le()




