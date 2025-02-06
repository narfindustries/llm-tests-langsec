# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gif(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gif.Header(self._io, self, self._root)
        self.logical_screen_descriptor = Gif.LogicalScreenDescriptor(self._io, self, self._root)
        if self.logical_screen_descriptor.has_global_color_table:
            self._raw_global_color_table = self._io.read_bytes(self.logical_screen_descriptor.global_color_table_size)
            _io__raw_global_color_table = KaitaiStream(BytesIO(self._raw_global_color_table))
            self.global_color_table = Gif.ColorTable(_io__raw_global_color_table, self, self._root)

        self.blocks = []
        i = 0
        while True:
            _ = Gif.Block(self._io, self, self._root)
            self.blocks.append(_)
            if  ((self._io.is_eof()) or (_.block_type == 59)) :
                break
            i += 1

    class ImageDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.left = self._io.read_u2le()
            self.top = self._io.read_u2le()
            self.width = self._io.read_u2le()
            self.height = self._io.read_u2le()
            self.flags = self._io.read_u1()

        @property
        def has_local_color_table(self):
            if hasattr(self, '_m_has_local_color_table'):
                return self._m_has_local_color_table

            self._m_has_local_color_table = (self.flags & 128) != 0
            return getattr(self, '_m_has_local_color_table', None)

        @property
        def is_interlaced(self):
            if hasattr(self, '_m_is_interlaced'):
                return self._m_is_interlaced

            self._m_is_interlaced = (self.flags & 64) != 0
            return getattr(self, '_m_is_interlaced', None)

        @property
        def is_sorted(self):
            if hasattr(self, '_m_is_sorted'):
                return self._m_is_sorted

            self._m_is_sorted = (self.flags & 32) != 0
            return getattr(self, '_m_is_sorted', None)

        @property
        def local_color_table_size(self):
            if hasattr(self, '_m_local_color_table_size'):
                return self._m_local_color_table_size

            self._m_local_color_table_size = (2 << (self.flags & 7))
            return getattr(self, '_m_local_color_table_size', None)


    class Rgb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.r = self._io.read_u1()
            self.g = self._io.read_u1()
            self.b = self._io.read_u1()


    class ImageData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.lzw_min_code_size = self._io.read_u1()
            self.blocks = []
            i = 0
            while True:
                _ = Gif.Subblock(self._io, self, self._root)
                self.blocks.append(_)
                if _.size == 0:
                    break
                i += 1


    class LogicalScreenDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.width = self._io.read_u2le()
            self.height = self._io.read_u2le()
            self.flags = self._io.read_u1()
            self.bg_color_index = self._io.read_u1()
            self.pixel_aspect_ratio = self._io.read_u1()

        @property
        def has_global_color_table(self):
            if hasattr(self, '_m_has_global_color_table'):
                return self._m_has_global_color_table

            self._m_has_global_color_table = (self.flags & 128) != 0
            return getattr(self, '_m_has_global_color_table', None)

        @property
        def color_resolution(self):
            if hasattr(self, '_m_color_resolution'):
                return self._m_color_resolution

            self._m_color_resolution = ((self.flags >> 4) & 7)
            return getattr(self, '_m_color_resolution', None)

        @property
        def is_sorted(self):
            if hasattr(self, '_m_is_sorted'):
                return self._m_is_sorted

            self._m_is_sorted = (self.flags & 8) != 0
            return getattr(self, '_m_is_sorted', None)

        @property
        def global_color_table_size(self):
            if hasattr(self, '_m_global_color_table_size'):
                return self._m_global_color_table_size

            self._m_global_color_table_size = (2 << (self.flags & 7))
            return getattr(self, '_m_global_color_table_size', None)


    class ExtensionBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.label = self._io.read_u1()
            _on = self.label
            if _on == 249:
                self.body = Gif.GraphicControlExtension(self._io, self, self._root)
            elif _on == 254:
                self.body = Gif.CommentExtension(self._io, self, self._root)
            elif _on == 1:
                self.body = Gif.PlainTextExtension(self._io, self, self._root)
            elif _on == 255:
                self.body = Gif.ApplicationExtension(self._io, self, self._root)


    class ImageBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_descriptor = Gif.ImageDescriptor(self._io, self, self._root)
            if self.image_descriptor.has_local_color_table:
                self._raw_local_color_table = self._io.read_bytes(self.image_descriptor.local_color_table_size)
                _io__raw_local_color_table = KaitaiStream(BytesIO(self._raw_local_color_table))
                self.local_color_table = Gif.ColorTable(_io__raw_local_color_table, self, self._root)

            self.image_data = Gif.ImageData(self._io, self, self._root)


    class CommentExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.comment_data = Gif.Subblock(self._io, self, self._root)


    class Block(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_type = self._io.read_u1()
            _on = self.block_type
            if _on == 44:
                self.block_body = Gif.ImageBlock(self._io, self, self._root)
            elif _on == 33:
                self.block_body = Gif.ExtensionBlock(self._io, self, self._root)
            elif _on == 59:
                self.block_body = Gif.Trailer(self._io, self, self._root)


    class Trailer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.terminator = self._io.read_bytes(1)
            if not self.terminator == b"\x3B":
                raise kaitaistruct.ValidationNotEqualError(b"\x3B", self.terminator, self._io, u"/types/trailer/seq/0")


    class ColorTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            for i in range(self._root.logical_screen_descriptor.global_color_table_size):
                self.entries.append(Gif.Rgb(self._io, self, self._root))



    class ApplicationExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x0B":
                raise kaitaistruct.ValidationNotEqualError(b"\x0B", self.block_size, self._io, u"/types/application_extension/seq/0")
            self.application_identifier = (self._io.read_bytes(8)).decode(u"ASCII")
            self.application_auth_code = (self._io.read_bytes(3)).decode(u"ASCII")
            self.application_data = Gif.Subblock(self._io, self, self._root)


    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.signature = self._io.read_bytes(3)
            if not self.signature == b"\x47\x49\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x47\x49\x46", self.signature, self._io, u"/types/header/seq/0")
            self.version = (self._io.read_bytes(3)).decode(u"ASCII")


    class Subblock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size = self._io.read_u1()
            self.bytes = self._io.read_bytes(self.size)


    class GraphicControlExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x04":
                raise kaitaistruct.ValidationNotEqualError(b"\x04", self.block_size, self._io, u"/types/graphic_control_extension/seq/0")
            self.flags = self._io.read_u1()
            self.delay_time = self._io.read_u2le()
            self.transparent_color_index = self._io.read_u1()
            self.terminator = self._io.read_bytes(1)
            if not self.terminator == b"\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x00", self.terminator, self._io, u"/types/graphic_control_extension/seq/4")

        @property
        def has_transparent_color(self):
            if hasattr(self, '_m_has_transparent_color'):
                return self._m_has_transparent_color

            self._m_has_transparent_color = (self.flags & 1) != 0
            return getattr(self, '_m_has_transparent_color', None)

        @property
        def user_input_flag(self):
            if hasattr(self, '_m_user_input_flag'):
                return self._m_user_input_flag

            self._m_user_input_flag = (self.flags & 2) != 0
            return getattr(self, '_m_user_input_flag', None)

        @property
        def disposal_method(self):
            if hasattr(self, '_m_disposal_method'):
                return self._m_disposal_method

            self._m_disposal_method = ((self.flags >> 2) & 7)
            return getattr(self, '_m_disposal_method', None)


    class PlainTextExtension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x0C":
                raise kaitaistruct.ValidationNotEqualError(b"\x0C", self.block_size, self._io, u"/types/plain_text_extension/seq/0")
            self.text_grid_left = self._io.read_u2le()
            self.text_grid_top = self._io.read_u2le()
            self.text_grid_width = self._io.read_u2le()
            self.text_grid_height = self._io.read_u2le()
            self.char_cell_width = self._io.read_u1()
            self.char_cell_height = self._io.read_u1()
            self.fg_color_index = self._io.read_u1()
            self.bg_color_index = self._io.read_u1()
            self.plain_text_data = Gif.Subblock(self._io, self, self._root)



