# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Nitf.NitfHeader(self._io, self, self._root)
        self.image_segments = []
        i = 0
        while not self._io.is_eof():
            self.image_segments.append(Nitf.NitfImageSegment(self._io, self, self._root))
            i += 1


    class NitfHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_header = Nitf.NitfFileHeader(self._io, self, self._root)
            self.image_subheaders = []
            i = 0
            while not self._io.is_eof():
                self.image_subheaders.append(Nitf.NitfImageSubheader(self._io, self, self._root))
                i += 1



    class NitfImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.NitfImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes_full()


    class NitfImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_id = self._io.read_bytes(10)
            self.image_date_and_time = self._io.read_bytes(14)
            self.target_id = self._io.read_bytes(17)
            self.image_title = self._io.read_bytes(80)
            self.image_security = Nitf.NitfSecurity(self._io, self, self._root)
            self.image_compression = self._io.read_bytes(2)
            self.image_representation = self._io.read_bytes(8)
            self.image_category = self._io.read_bytes(8)
            self.image_data = self._io.read_bytes_full()


    class NitfSecurity(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.classification = self._io.read_bytes(1)
            self.codewords = self._io.read_bytes(40)
            self.control_and_handling = self._io.read_bytes(40)
            self.releaseability = self._io.read_bytes(20)
            self.declass_type = self._io.read_bytes(2)
            self.declass_date = self._io.read_bytes(8)
            self.declass_exemption = self._io.read_bytes(4)
            self.downgrade = self._io.read_bytes(1)
            self.downgrade_date = self._io.read_bytes(8)
            self.classification_text = self._io.read_bytes(43)
            self.classification_authority_type = self._io.read_bytes(1)
            self.classification_authority = self._io.read_bytes(40)
            self.classification_reason = self._io.read_bytes(1)
            self.security_source_date = self._io.read_bytes(8)
            self.security_control_number = self._io.read_bytes(15)


    class NitfFileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = self._io.read_bytes(4)
            self.file_version = self._io.read_bytes(5)
            self.complex_level = self._io.read_bytes(2)
            self.standard_type = self._io.read_bytes(4)
            self.originating_station_id = self._io.read_bytes(10)
            self.file_date_and_time = self._io.read_bytes(14)
            self.file_title = self._io.read_bytes(80)
            self.file_security = Nitf.NitfSecurity(self._io, self, self._root)
            self.file_copy_number = self._io.read_bytes(5)
            self.file_number_of_copies = self._io.read_bytes(5)
            self.encryption = self._io.read_bytes(1)
            self.file_background_color = self._io.read_bytes(3)
            self.originator_name = self._io.read_bytes(24)
            self.originator_phone = self._io.read_bytes(18)



