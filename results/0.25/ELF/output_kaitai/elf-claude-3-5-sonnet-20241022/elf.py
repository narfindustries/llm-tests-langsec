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

    class ShType(Enum):
        null_type = 0
        progbits = 1
        symtab = 2
        strtab = 3
        rela = 4
        hash = 5
        dynamic = 6
        note = 7
        nobits = 8
        rel = 9
        shlib = 10
        dynsym = 11
        init_array = 14
        fini_array = 15
        preinit_array = 16
        group = 17
        symtab_shndx = 18
        loos = 1610612736
        hios = 1879048191
        loproc = 1879048192
        hiproc = 2147483647

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
        none = 0
        m32 = 1
        sparc = 2
        i386 = 3
        m68k = 4
        m88k = 5
        i860 = 7
        mips = 8
        x86_64 = 62

    class Bits(Enum):
        b32 = 1
        b64 = 2

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

    class ObjType(Enum):
        none = 0
        relocatable = 1
        executable = 2
        shared = 3
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
        self.bits = KaitaiStream.resolve_enum(Elf.Bits, self._io.read_u1())
        self.endian = KaitaiStream.resolve_enum(Elf.Endian, self._io.read_u1())
        self.ei_version = self._io.read_u1()
        self.abi = KaitaiStream.resolve_enum(Elf.OsAbi, self._io.read_u1())
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.version = self._io.read_u4le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.entry_point = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.entry_point = self._io.read_u8le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.program_header_offset = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.program_header_offset = self._io.read_u8le()
        _on = self.bits
        if _on == Elf.Bits.b32:
            self.section_header_offset = self._io.read_u4le()
        elif _on == Elf.Bits.b64:
            self.section_header_offset = self._io.read_u8le()
        self.flags = self._io.read_u4le()
        self.header_size = self._io.read_u2le()
        self.program_header_entry_size = self._io.read_u2le()
        self.program_header_num_entries = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.section_header_num_entries = self._io.read_u2le()
        self.section_names_idx = self._io.read_u2le()
        self.program_headers = []
        for i in range(self.program_header_num_entries):
            self.program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

        self.section_headers = []
        for i in range(self.section_header_num_entries):
            self.section_headers.append(Elf.SectionHeader(self._io, self, self._root))


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.PhType, self._io.read_u4le())
            if self._root.bits == Elf.Bits.b64:
                self.flags = self._io.read_u4le()

            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.vaddr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.vaddr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.paddr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.paddr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.filesz = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.filesz = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.memsz = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.memsz = self._io.read_u8le()
            if self._root.bits == Elf.Bits.b32:
                self.flags_32 = self._io.read_u4le()

            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.align = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.align = self._io.read_u8le()


    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = KaitaiStream.resolve_enum(Elf.ShType, self._io.read_u4le())
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.flags = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.flags = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.addr = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.addr = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.offset = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.offset = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.size = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.size = self._io.read_u8le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.align = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.align = self._io.read_u8le()
            _on = self._root.bits
            if _on == Elf.Bits.b32:
                self.entry_size = self._io.read_u4le()
            elif _on == Elf.Bits.b64:
                self.entry_size = self._io.read_u8le()



