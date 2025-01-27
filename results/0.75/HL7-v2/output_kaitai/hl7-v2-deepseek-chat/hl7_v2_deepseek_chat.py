# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Hl7V2DeepseekChat(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Hl7V2DeepseekChat.MshSegment(self._io, self, self._root)
        self.segments = []
        i = 0
        while not self._io.is_eof():
            self.segments.append(Hl7V2DeepseekChat.Segment(self._io, self, self._root))
            i += 1


    class MshSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_separator = (self._io.read_bytes(1)).decode(u"UTF-8")
            self.encoding_characters = (self._io.read_bytes(4)).decode(u"UTF-8")
            self.sending_application = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.sending_facility = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.receiving_application = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.receiving_facility = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.date_time_of_message = (self._io.read_bytes(26)).decode(u"UTF-8")
            self.security = (self._io.read_bytes(40)).decode(u"UTF-8")
            self.message_type = (self._io.read_bytes(7)).decode(u"UTF-8")
            self.message_control_id = (self._io.read_bytes(20)).decode(u"UTF-8")
            self.processing_id = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.version_id = (self._io.read_bytes(7)).decode(u"UTF-8")
            self.sequence_number = (self._io.read_bytes(15)).decode(u"UTF-8")
            self.continuation_pointer = (self._io.read_bytes(180)).decode(u"UTF-8")
            self.accept_acknowledgment_type = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.application_acknowledgment_type = (self._io.read_bytes(2)).decode(u"UTF-8")
            self.country_code = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.character_set = (self._io.read_bytes(16)).decode(u"UTF-8")
            self.principal_language_of_message = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.alternate_character_set_handling_scheme = (self._io.read_bytes(20)).decode(u"UTF-8")


    class Segment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.segment_id = (self._io.read_bytes(3)).decode(u"UTF-8")
            self.fields = []
            i = 0
            while not self._io.is_eof():
                self.fields.append(Hl7V2DeepseekChat.Field(self._io, self, self._root))
                i += 1



    class Field(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.field_value = (self._io.read_bytes_full()).decode(u"UTF-8")



