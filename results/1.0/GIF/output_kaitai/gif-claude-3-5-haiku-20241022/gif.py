# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO
from enum import Enum


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Gif(KaitaiStruct):

    class BlockType(Enum):
        extension = 33
        image_descriptor = 44
        trailer = 59

    class ExtensionType(Enum):
        plain_text_extension = 1
        graphic_control_extension = 249
        comment_extension = 254
        application_extension = 255
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Gif.Header(self._io, self, self._root)
        self.logical_screen_descriptor = Gif.LogicalScreenDescriptor(self._io, self, self._root)
        if self.logical_screen_descriptor.global_color_table_flag:
            self._raw_global_color_table = self._io.read_bytes((self.logical_screen_descriptor.global_color_table_size * 3))
            _io__raw_global_color_table = KaitaiStream(BytesIO(self._raw_global_color_table))
            self.global_color_table = Gif.ColorTable(_io__raw_global_color_table, self, self._root)

        self.blocks = []
        i = 0
        while True:
            _ = Gif.Block(self._io, self, self._root)
            self.blocks.append(_)
            if _.block_type == Gif.BlockType.trailer:
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
            if self.local_color_table_flag:
                self._raw_local_color_table = self._io.read_bytes((self.local_color_table_size * 3))
                _io__raw_local_color_table = KaitaiStream(BytesIO(self._raw_local_color_table))
                self.local_color_table = Gif.ColorTable(_io__raw_local_color_table, self, self._root)

            self.lzw_min_code_size = self._io.read_u1()
            self.image_data = Gif.ImageData(self._io, self, self._root)

        @property
        def local_color_table_flag(self):
            if hasattr(self, '_m_local_color_table_flag'):
                return self._m_local_color_table_flag

            self._m_local_color_table_flag = (self.flags & 128) != 0
            return getattr(self, '_m_local_color_table_flag', None)

        @property
        def local_color_table_size(self):
            if hasattr(self, '_m_local_color_table_size'):
                return self._m_local_color_table_size

            self._m_local_color_table_size = (self.flags & 7)
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
            self.blocks = []
            i = 0
            while True:
                _ = Gif.DataSubBlock(self._io, self, self._root)
                self.blocks.append(_)
                if _.block_terminator == 0:
                    break
                i += 1


    class LogicalScreenDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.canvas_width = self._io.read_u2le()
            self.canvas_height = self._io.read_u2le()
            self.flags = self._io.read_u1()
            self.bg_color_index = self._io.read_u1()
            self.pixel_aspect_ratio = self._io.read_u1()

        @property
        def global_color_table_flag(self):
            if hasattr(self, '_m_global_color_table_flag'):
                return self._m_global_color_table_flag

            self._m_global_color_table_flag = (self.flags & 128) != 0
            return getattr(self, '_m_global_color_table_flag', None)

        @property
        def global_color_table_size(self):
            if hasattr(self, '_m_global_color_table_size'):
                return self._m_global_color_table_size

            self._m_global_color_table_size = (self.flags & 7)
            return getattr(self, '_m_global_color_table_size', None)


    class ExtensionBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = KaitaiStream.resolve_enum(Gif.ExtensionType, self._io.read_u1())
            self.data = self._io.read_bytes_full()


    class Block(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_type = KaitaiStream.resolve_enum(Gif.BlockType, self._io.read_u1())

        @property
        def extension(self):
            if hasattr(self, '_m_extension'):
                return self._m_extension

            if self.block_type == Gif.BlockType.extension:
                self._m_extension = Gif.ExtensionBlock(self._io, self, self._root)

            return getattr(self, '_m_extension', None)

        @property
        def image_descriptor(self):
            if hasattr(self, '_m_image_descriptor'):
                return self._m_image_descriptor

            if self.block_type == Gif.BlockType.image_descriptor:
                self._m_image_descriptor = Gif.ImageDescriptor(self._io, self, self._root)

            return getattr(self, '_m_image_descriptor', None)


    class DataSubBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_u1()
            self.block_terminator = self._io.read_u1()
            if self.block_size > 0:
                self.block_data = self._io.read_bytes(self.block_size)



    class ColorTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            for i in range(256):
                self.entries.append(Gif.Rgb(self._io, self, self._root))



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
            self.version = (self._io.read_bytes(3)).decode(u"ascii")



