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
        self.file_header = Nitf.FileHeader(self._io, self, self._root)

    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_type = self._io.read_bytes(4)
            if not self.file_type == b"\x4E\x49\x54\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x4E\x49\x54\x46", self.file_type, self._io, u"/types/file_header/seq/0")
            self.version = self._io.read_bytes(5)
            self.complexity_level = self._io.read_bytes(2)
            self.st_type = self._io.read_bytes(4)
            self.ostanag_version = self._io.read_bytes(4)
            self.classification = self._io.read_bytes(1)
            self.security_classification_system = self._io.read_bytes(2)
            self.codewords = self._io.read_bytes(11)
            self.control_and_handling = self._io.read_bytes(2)
            self.release_instructions = self._io.read_bytes(20)
            self.declassification_type = self._io.read_bytes(2)
            self.declassification_date = self._io.read_bytes(8)
            self.declassification_exemption = self._io.read_bytes(4)
            self.downgrade = self._io.read_bytes(1)
            self.downgrade_date = self._io.read_bytes(8)
            self.classification_text = self._io.read_bytes(43)
            self.classification_authority_type = self._io.read_bytes(1)
            self.classification_authority = self._io.read_bytes(40)
            self.classification_reason = self._io.read_bytes(1)
            self.security_source_date = self._io.read_bytes(8)
            self.security_control_number = self._io.read_bytes(15)
            self.copy_number = self._io.read_bytes(5)
            self.number_of_copies = self._io.read_bytes(5)
            self.encryption = self._io.read_bytes(1)
            self.file_date_time = self._io.read_bytes(14)
            self.file_title = self._io.read_bytes(80)
            self.file_type_id = self._io.read_bytes(35)
            self.file_type_description = self._io.read_bytes(35)
            self.file_structure = self._io.read_bytes(1)
            self.file_header_length = self._io.read_bytes(6)
            self.file_data_length = self._io.read_bytes(10)
            self.number_of_image_segments = self._io.read_bytes(3)
            self.number_of_graphics_segments = self._io.read_bytes(3)
            self.number_of_text_files = self._io.read_bytes(3)
            self.number_of_data_extension_segments = self._io.read_bytes(3)
            self.number_of_reserved_extension_segments = self._io.read_bytes(3)
            self.user_defined_header_data_length = self._io.read_bytes(5)
            self.user_defined_header_overflow = self._io.read_bytes(3)
            self.extended_header_data_length = self._io.read_bytes(5)
            self.extended_header_overflow = self._io.read_bytes(3)



