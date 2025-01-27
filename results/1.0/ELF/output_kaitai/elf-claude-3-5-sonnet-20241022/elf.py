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

    class OsAbi(Enum):
        system_v = 0
        hp_ux = 1
        netbsd = 2
        linux = 3
        hurd = 4
        solaris = 6
        aix = 7
        irix = 8
        freebsd = 9
        tru64 = 10
        modesto = 11
        openbsd = 12
        openvms = 13
        nsk = 14
        aros = 15
        fenixos = 16
        cloudabi = 17
        openvos = 18

    class Machine(Enum):
        none = 0
        m32 = 1
        sparc = 2
        x86 = 3
        m68k = 4
        m88k = 5
        i860 = 6
        mips = 7
        s370 = 8
        mips_rs3_le = 9
        sparc64_old = 10
        i960 = 11
        powerpc = 12
        powerpc64 = 13
        s390 = 14
        spu = 15
        arm = 19
        sh = 20
        sparcv9 = 21
        h8_300 = 22
        ia64 = 23
        sh64 = 24
        mips64 = 25
        s390x = 26
        aarch64 = 40
        riscv = 42
        x86_64 = 62
        arm64 = 183

    class Bits(Enum):
        b32 = 1
        b64 = 2

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
        self.file_type = KaitaiStream.resolve_enum(Elf.ObjType, self._io.read_u2le())
        self.machine = KaitaiStream.resolve_enum(Elf.Machine, self._io.read_u2le())
        self.e_version = self._io.read_u4le()
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
        self.qty_program_header = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.qty_section_header = self._io.read_u2le()
        self.section_names_idx = self._io.read_u2le()

    class ProgramHeader(KaitaiStruct):

        class PhType(Enum):
            null_type = 0
            load = 1
            dynamic = 2
            interp = 3
            note = 4
            shlib = 5
            phdr = 6
            tls = 7
            loos = 1610612736
            hios = 1879048191
            loproc = 1879048192
            hiproc = 2147483647
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = KaitaiStream.resolve_enum(Elf.ProgramHeader.PhType, self._io.read_u4le())
            if self._root.bits == Elf.Bits.b64:
                self.flags_64 = self._io.read_u4le()

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
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = KaitaiStream.resolve_enum(Elf.SectionHeader.ShType, self._io.read_u4le())
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

        @property
        def name(self):
            if hasattr(self, '_m_name'):
                return self._m_name

            io = self._root._io
            _pos = io.pos()
            io.seek((self._root.section_headers[self._root.section_names_idx].offset + self.name_offset))
            self._m_name = (io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            io.seek(_pos)
            return getattr(self, '_m_name', None)

        @property
        def body(self):
            if hasattr(self, '_m_body'):
                return self._m_body

            io = self._root._io
            _pos = io.pos()
            io.seek(self.offset)
            self._m_body = io.read_bytes(self.size)
            io.seek(_pos)
            return getattr(self, '_m_body', None)


    @property
    def program_headers(self):
        if hasattr(self, '_m_program_headers'):
            return self._m_program_headers

        _pos = self._io.pos()
        self._io.seek(self.program_header_offset)
        self._m_program_headers = []
        for i in range(self.qty_program_header):
            self._m_program_headers.append(Elf.ProgramHeader(self._io, self, self._root))

        self._io.seek(_pos)
        return getattr(self, '_m_program_headers', None)

    @property
    def section_headers(self):
        if hasattr(self, '_m_section_headers'):
            return self._m_section_headers

        _pos = self._io.pos()
        self._io.seek(self.section_header_offset)
        self._m_section_headers = []
        for i in range(self.qty_section_header):
            self._m_section_headers.append(Elf.SectionHeader(self._io, self, self._root))

        self._io.seek(_pos)
        return getattr(self, '_m_section_headers', None)


