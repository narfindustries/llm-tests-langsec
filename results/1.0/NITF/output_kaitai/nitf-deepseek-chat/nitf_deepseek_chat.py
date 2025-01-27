# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class NitfDeepseekChat(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.file_header = NitfDeepseekChat.FileHeader(self._io, self, self._root)
        self.image_segments = []
        i = 0
        while not self._io.is_eof():
            self.image_segments.append(NitfDeepseekChat.ImageSegment(self._io, self, self._root))
            i += 1

        self.text_segments = []
        i = 0
        while not self._io.is_eof():
            self.text_segments.append(NitfDeepseekChat.TextSegment(self._io, self, self._root))
            i += 1

        self.end_of_file_marker = self._io.read_bytes(4)
        if not self.end_of_file_marker == b"\x45\x4F\x46\x00":
            raise kaitaistruct.ValidationNotEqualError(b"\x45\x4F\x46\x00", self.end_of_file_marker, self._io, u"/seq/3")

    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader_length = self._io.read_u4be()
            self.text_id = self._io.read_bytes(10)
            self.text_date_and_time = self._io.read_bytes(14)
            self.text_title = self._io.read_bytes(80)
            self.text_security_classification = self._io.read_bytes(1)
            self.text_security_system = self._io.read_bytes(2)
            self.text_codewords = self._io.read_bytes(11)
            self.text_control_and_handling = self._io.read_bytes(2)
            self.text_releaseability = self._io.read_bytes(20)
            self.text_declass_type = self._io.read_bytes(2)
            self.text_declass_date = self._io.read_bytes(8)
            self.text_declass_exemption = self._io.read_bytes(4)
            self.text_downgrade = self._io.read_bytes(1)
            self.text_downgrade_date = self._io.read_bytes(8)
            self.text_classification_text = self._io.read_bytes(43)
            self.text_classification_authority_type = self._io.read_bytes(1)
            self.text_classification_authority = self._io.read_bytes(40)
            self.text_classification_reason = self._io.read_bytes(1)
            self.text_security_source_date = self._io.read_bytes(8)
            self.text_security_control_number = self._io.read_bytes(15)
            self.text_data_length = self._io.read_u4be()


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = NitfDeepseekChat.ImageSubheader(self._io, self, self._root)
            self.image_data = self._io.read_bytes(self.image_subheader.image_data_length)


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader = NitfDeepseekChat.TextSubheader(self._io, self, self._root)
            self.text_data = self._io.read_bytes(self.text_subheader.text_data_length)


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader_length = self._io.read_u4be()
            self.image_id = self._io.read_bytes(10)
            self.image_date_and_time = self._io.read_bytes(14)
            self.image_title = self._io.read_bytes(80)
            self.image_security_classification = self._io.read_bytes(1)
            self.image_security_system = self._io.read_bytes(2)
            self.image_codewords = self._io.read_bytes(11)
            self.image_control_and_handling = self._io.read_bytes(2)
            self.image_releaseability = self._io.read_bytes(20)
            self.image_declass_type = self._io.read_bytes(2)
            self.image_declass_date = self._io.read_bytes(8)
            self.image_declass_exemption = self._io.read_bytes(4)
            self.image_downgrade = self._io.read_bytes(1)
            self.image_downgrade_date = self._io.read_bytes(8)
            self.image_classification_text = self._io.read_bytes(43)
            self.image_classification_authority_type = self._io.read_bytes(1)
            self.image_classification_authority = self._io.read_bytes(40)
            self.image_classification_reason = self._io.read_bytes(1)
            self.image_security_source_date = self._io.read_bytes(8)
            self.image_security_control_number = self._io.read_bytes(15)
            self.image_data_length = self._io.read_u4be()


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = self._io.read_bytes(4)
            if not self.file_profile_name == b"\x4E\x49\x54\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x4E\x49\x54\x46", self.file_profile_name, self._io, u"/types/file_header/seq/0")
            self.file_version = self._io.read_bytes(5)
            if not self.file_version == b"\x30\x32\x2E\x31\x30":
                raise kaitaistruct.ValidationNotEqualError(b"\x30\x32\x2E\x31\x30", self.file_version, self._io, u"/types/file_header/seq/1")
            self.complexity_level = self._io.read_u1()
            self.standard_type = self._io.read_bytes(2)
            self.originating_station_id = self._io.read_bytes(10)
            self.file_date_and_time = self._io.read_bytes(14)
            self.file_title = self._io.read_bytes(80)
            self.security_classification = self._io.read_bytes(1)
            self.security_system = self._io.read_bytes(2)
            self.codewords = self._io.read_bytes(11)
            self.control_and_handling = self._io.read_bytes(2)
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



