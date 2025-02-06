# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(4)
        if not self.magic == b"\x7F\x45\x4C\x46":
            raise kaitaistruct.ValidationNotEqualError(b"\x7F\x45\x4C\x46", self.magic, self._io, u"/seq/0")
        self.bits = self._io.read_u1()
        self.endian = self._io.read_u1()
        self.ei_version = self._io.read_bytes(1)
        if not self.ei_version == b"\x01":
            raise kaitaistruct.ValidationNotEqualError(b"\x01", self.ei_version, self._io, u"/seq/3")
        self.os_abi = self._io.read_u1()
        self.abi_version = self._io.read_u1()
        self.pad = self._io.read_bytes(7)
        self.e_type = self._io.read_u2le()
        self.machine = self._io.read_u2le()
        self.version = self._io.read_u4le()
        _on = self.bits
        if _on == 1:
            self.entry_point = self._io.read_u4le()
        elif _on == 2:
            self.entry_point = self._io.read_u8le()
        _on = self.bits
        if _on == 1:
            self.program_header_offset = self._io.read_u4le()
        elif _on == 2:
            self.program_header_offset = self._io.read_u8le()
        _on = self.bits
        if _on == 1:
            self.section_header_offset = self._io.read_u4le()
        elif _on == 2:
            self.section_header_offset = self._io.read_u8le()
        self.flags = self._io.read_u4le()
        self.header_size = self._io.read_u2le()
        self.program_header_entry_size = self._io.read_u2le()
        self.program_header_entry_count = self._io.read_u2le()
        self.section_header_entry_size = self._io.read_u2le()
        self.section_header_entry_count = self._io.read_u2le()
        self.section_names_idx = self._io.read_u2le()

    class ProgramHeader(KaitaiStruct):
        def __init__(self, bits, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self.bits = bits
            self._read()

        def _read(self):
            self.type = self._io.read_u4le()
            self.flags = self._io.read_u4le()
            _on = self.bits
            if _on == 1:
                self.offset = self._io.read_u4le()
            elif _on == 2:
                self.offset = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.vaddr = self._io.read_u4le()
            elif _on == 2:
                self.vaddr = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.paddr = self._io.read_u4le()
            elif _on == 2:
                self.paddr = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.filesz = self._io.read_u4le()
            elif _on == 2:
                self.filesz = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.memsz = self._io.read_u4le()
            elif _on == 2:
                self.memsz = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.align = self._io.read_u4le()
            elif _on == 2:
                self.align = self._io.read_u8le()


    class SectionHeader(KaitaiStruct):
        def __init__(self, bits, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self.bits = bits
            self._read()

        def _read(self):
            self.name_offset = self._io.read_u4le()
            self.type = self._io.read_u4le()
            _on = self.bits
            if _on == 1:
                self.flags = self._io.read_u4le()
            elif _on == 2:
                self.flags = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.addr = self._io.read_u4le()
            elif _on == 2:
                self.addr = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.offset = self._io.read_u4le()
            elif _on == 2:
                self.offset = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.size = self._io.read_u4le()
            elif _on == 2:
                self.size = self._io.read_u8le()
            self.link = self._io.read_u4le()
            self.info = self._io.read_u4le()
            _on = self.bits
            if _on == 1:
                self.addr_align = self._io.read_u4le()
            elif _on == 2:
                self.addr_align = self._io.read_u8le()
            _on = self.bits
            if _on == 1:
                self.entry_size = self._io.read_u4le()
            elif _on == 2:
                self.entry_size = self._io.read_u8le()


    @property
    def program_headers(self):
        if hasattr(self, '_m_program_headers'):
            return self._m_program_headers

        _pos = self._io.pos()
        self._io.seek(self.program_header_offset)
        self._m_program_headers = []
        for i in range(self.program_header_entry_count):
            self._m_program_headers.append(Elf.ProgramHeader(self.bits, self._io, self, self._root))

        self._io.seek(_pos)
        return getattr(self, '_m_program_headers', None)

    @property
    def section_headers(self):
        if hasattr(self, '_m_section_headers'):
            return self._m_section_headers

        _pos = self._io.pos()
        self._io.seek(self.section_header_offset)
        self._m_section_headers = []
        for i in range(self.section_header_entry_count):
            self._m_section_headers.append(Elf.SectionHeader(self.bits, self._io, self, self._root))

        self._io.seek(_pos)
        return getattr(self, '_m_section_headers', None)


