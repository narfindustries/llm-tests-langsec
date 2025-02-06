# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gif(KaitaiStruct):

    class BlockType(Enum):
        extension = 33
        image = 44
        end_of_file = 59

    class ExtensionType(Enum):
        plain_text = 1
        graphics_control = 249
        comment = 254
        application = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gif.Header(self._io, self, self._root)
        self.logical_screen = Gif.LogicalScreenDescriptor(self._io, self, self._root)
        if self.logical_screen.has_global_color_table:
            self._raw_global_color_table = self._io.read_bytes(self.logical_screen.global_color_table_size)
            _io__raw_global_color_table = KaitaiStream(BytesIO(self._raw_global_color_table))
            self.global_color_table = Gif.ColorTable(_io__raw_global_color_table, self, self._root)

        self.blocks = []
        i = 0
        while True:
            _ = Gif.Block(self._io, self, self._root)
            self.blocks.append(_)
            if _.block_type == 59:
                break
            i += 1

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
            self.subblocks = Gif.Subblocks(self._io, self, self._root)


    class LogicalScreenDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.screen_width = self._io.read_u2le()
            self.screen_height = self._io.read_u2le()
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

            self._m_color_resolution = ((self.flags & 112) >> 4)
            return getattr(self, '_m_color_resolution', None)

        @property
        def sort_flag(self):
            if hasattr(self, '_m_sort_flag'):
                return self._m_sort_flag

            self._m_sort_flag = (self.flags & 8) != 0
            return getattr(self, '_m_sort_flag', None)

        @property
        def global_color_table_size(self):
            if hasattr(self, '_m_global_color_table_size'):
                return self._m_global_color_table_size

            self._m_global_color_table_size = (3 * (1 << ((self.flags & 7) + 1)))
            return getattr(self, '_m_global_color_table_size', None)


    class ExtensionBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = self._io.read_u1()
            _on = self.extension_type
            if _on == 249:
                self.body = Gif.GraphicsControlExt(self._io, self, self._root)
            elif _on == 254:
                self.body = Gif.CommentExt(self._io, self, self._root)
            elif _on == 1:
                self.body = Gif.PlainTextExt(self._io, self, self._root)
            elif _on == 255:
                self.body = Gif.ApplicationExt(self._io, self, self._root)


    class ImageBlock(KaitaiStruct):
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
            if self.has_local_color_table:
                self._raw_local_color_table = self._io.read_bytes(self.len_local_color_table)
                _io__raw_local_color_table = KaitaiStream(BytesIO(self._raw_local_color_table))
                self.local_color_table = Gif.ColorTable(_io__raw_local_color_table, self, self._root)

            self.image_data = Gif.ImageData(self._io, self, self._root)

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
        def len_local_color_table(self):
            if hasattr(self, '_m_len_local_color_table'):
                return self._m_len_local_color_table

            self._m_len_local_color_table = (3 * (1 << ((self.flags & 7) + 1)))
            return getattr(self, '_m_len_local_color_table', None)


    class GraphicsControlExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x04":
                raise kaitaistruct.ValidationNotEqualError(b"\x04", self.block_size, self._io, u"/types/graphics_control_ext/seq/0")
            self.flags = self._io.read_u1()
            self.delay_time = self._io.read_u2le()
            self.transparent_idx = self._io.read_u1()
            self.terminator = self._io.read_bytes(1)
            if not self.terminator == b"\x00":
                raise kaitaistruct.ValidationNotEqualError(b"\x00", self.terminator, self._io, u"/types/graphics_control_ext/seq/4")

        @property
        def disposal_method(self):
            if hasattr(self, '_m_disposal_method'):
                return self._m_disposal_method

            self._m_disposal_method = ((self.flags & 28) >> 2)
            return getattr(self, '_m_disposal_method', None)

        @property
        def user_input(self):
            if hasattr(self, '_m_user_input'):
                return self._m_user_input

            self._m_user_input = (self.flags & 2) != 0
            return getattr(self, '_m_user_input', None)

        @property
        def transparent_color_flag(self):
            if hasattr(self, '_m_transparent_color_flag'):
                return self._m_transparent_color_flag

            self._m_transparent_color_flag = (self.flags & 1) != 0
            return getattr(self, '_m_transparent_color_flag', None)


    class Block(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_type = self._io.read_u1()
            _on = self.block_type
            if _on == 33:
                self.block_data = Gif.ExtensionBlock(self._io, self, self._root)
            elif _on == 44:
                self.block_data = Gif.ImageBlock(self._io, self, self._root)


    class PlainTextExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x0C":
                raise kaitaistruct.ValidationNotEqualError(b"\x0C", self.block_size, self._io, u"/types/plain_text_ext/seq/0")
            self.grid_left = self._io.read_u2le()
            self.grid_top = self._io.read_u2le()
            self.grid_width = self._io.read_u2le()
            self.grid_height = self._io.read_u2le()
            self.cell_width = self._io.read_u1()
            self.cell_height = self._io.read_u1()
            self.fg_color_index = self._io.read_u1()
            self.bg_color_index = self._io.read_u1()
            self.subblocks = Gif.Subblocks(self._io, self, self._root)


    class ColorTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while not self._io.is_eof():
                self.entries.append(Gif.Rgb(self._io, self, self._root))
                i += 1



    class Header(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.magic = self._io.read_bytes(3)
            if not self.magic == b"\x47\x49\x46":
                raise kaitaistruct.ValidationNotEqualError(b"\x47\x49\x46", self.magic, self._io, u"/types/header/seq/0")
            self.version = (self._io.read_bytes(3)).decode(u"ASCII")


    class ApplicationExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_bytes(1)
            if not self.block_size == b"\x0B":
                raise kaitaistruct.ValidationNotEqualError(b"\x0B", self.block_size, self._io, u"/types/application_ext/seq/0")
            self.application_id = (self._io.read_bytes(8)).decode(u"ASCII")
            self.auth_code = self._io.read_bytes(3)
            self.subblocks = Gif.Subblocks(self._io, self, self._root)


    class Subblock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.len_data = self._io.read_u1()
            self.data = self._io.read_bytes(self.len_data)


    class CommentExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.subblocks = Gif.Subblocks(self._io, self, self._root)


    class Subblocks(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            i = 0
            while True:
                _ = Gif.Subblock(self._io, self, self._root)
                self.entries.append(_)
                if _.len_data == 0:
                    break
                i += 1



