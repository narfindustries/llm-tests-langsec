# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Elf(KaitaiStruct):

    class ElfClass(Enum):
        invalid = 0
        class_32 = 1
        class_64 = 2

    class ElfData(Enum):
        invalid = 0
        lsb = 1
        msb = 2

    class ElfOsabi(Enum):
        sysv = 0
        hpux = 1
        netbsd = 2
        linux = 3
        gnu_hurd = 4
        solaris = 6
        aix = 7
        irix = 8
        freebsd = 9
        tru64 = 10
        novell_modesto = 11
        openbsd = 12
        openvms = 13
        nonstop_kernel = 14
        ar_os = 15
        fuchsia = 16
        cloud_abi = 17
        openvos = 18

    class PtType(Enum):
        null = 0
        load = 1
        dynamic = 2
        interp = 3
        note = 4
        shlib = 5
        phdr = 6
        loos = 1610612736
        hios = 1879048191
        loproc = 1879048192
        hiproc = 2147483647

    class ShtType(Enum):
        null = 0
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
        self.e_ident = Elf.EIdent(self._io, self, self._root)
        self.header = Elf.ElfHeader(self._io, self, self._root)

    class ElfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.e_type = self._io.read_u2be()
            self.e_machine = self._io.read_u2be()
            self.e_version = self._io.read_u4be()
            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.e_entry = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.e_entry_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.e_phoff = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.e_phoff_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.e_shoff = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.e_shoff_64 = self._io.read_bits_int_be(8)

            self._io.align_to_byte()
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
            self.ei_class = KaitaiStream.resolve_enum(Elf.ElfClass, self._io.read_u1())
            self.ei_data = KaitaiStream.resolve_enum(Elf.ElfData, self._io.read_u1())
            self.ei_version = self._io.read_u1()
            self.ei_osabi = KaitaiStream.resolve_enum(Elf.ElfOsabi, self._io.read_u1())
            self.ei_abiversion = self._io.read_u1()
            self.ei_pad = self._io.read_bytes(7)


    class ProgramHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.p_type = KaitaiStream.resolve_enum(Elf.PtType, self._io.read_u4be())
            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_offset = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_offset_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_vaddr = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_vaddr_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_paddr = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_paddr_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_filesz = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_filesz_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_memsz = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_memsz_64 = self._io.read_bits_int_be(8)

            self._io.align_to_byte()
            self.p_flags = self._io.read_u4be()
            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.p_align = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.p_align_64 = self._io.read_bits_int_be(8)



    class SectionHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sh_name = self._io.read_u4be()
            self.sh_type = KaitaiStream.resolve_enum(Elf.ShtType, self._io.read_u4be())
            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_flags = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_flags_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_addr = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_addr_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_offset = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_offset_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_size = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_size_64 = self._io.read_bits_int_be(8)

            self._io.align_to_byte()
            self.sh_link = self._io.read_u4be()
            self.sh_info = self._io.read_u4be()
            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_addralign = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_addralign_64 = self._io.read_bits_int_be(8)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_32:
                self.sh_entsize = self._io.read_bits_int_be(4)

            if self._root.e_ident.ei_class == Elf.ElfClass.class_64:
                self.sh_entsize_64 = self._io.read_bits_int_be(8)




