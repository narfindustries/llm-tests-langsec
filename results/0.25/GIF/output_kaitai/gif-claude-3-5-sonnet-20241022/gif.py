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
        self.logical_screen = Gif.LogicalScreen(self._io, self, self._root)
        self.blocks = []
        i = 0
        while True:
            _ = Gif.Block(self._io, self, self._root)
            self.blocks.append(_)
            if _.block_type == Gif.BlockType.end_of_file:
                break
            i += 1

    class ImageData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.lzw_min_code_size = self._io.read_u1()
            self.data = Gif.DataBlocks(self._io, self, self._root)


    class SkipBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = Gif.DataBlocks(self._io, self, self._root)


    class DataBlocks(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.blocks = []
            i = 0
            while True:
                _ = Gif.DataBlock(self._io, self, self._root)
                self.blocks.append(_)
                if _.block_size == 0:
                    break
                i += 1


    class ExtensionBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.extension_type = KaitaiStream.resolve_enum(Gif.ExtensionType, self._io.read_u1())
            _on = self.extension_type
            if _on == Gif.ExtensionType.graphics_control:
                self.extension_data = Gif.GraphicsControlExt(self._io, self, self._root)
            elif _on == Gif.ExtensionType.comment:
                self.extension_data = Gif.CommentExt(self._io, self, self._root)
            elif _on == Gif.ExtensionType.application:
                self.extension_data = Gif.ApplicationExt(self._io, self, self._root)
            else:
                self.extension_data = Gif.SkipBlock(self._io, self, self._root)


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
            self.image_data = Gif.ImageData(self._io, self, self._root)


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


    class LogicalScreen(KaitaiStruct):
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


    class Block(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_type = KaitaiStream.resolve_enum(Gif.BlockType, self._io.read_u1())
            _on = self.block_type
            if _on == Gif.BlockType.extension:
                self.block_data = Gif.ExtensionBlock(self._io, self, self._root)
            elif _on == Gif.BlockType.image:
                self.block_data = Gif.ImageBlock(self._io, self, self._root)


    class DataBlock(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.block_size = self._io.read_u1()
            self.data = self._io.read_bytes(self.block_size)


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
            self.data = Gif.DataBlocks(self._io, self, self._root)


    class CommentExt(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.data = Gif.DataBlocks(self._io, self, self._root)



