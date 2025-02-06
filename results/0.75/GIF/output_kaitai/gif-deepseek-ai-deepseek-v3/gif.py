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
        if self.logical_screen_descriptor.global_color_table_flag == 1:
            self.global_color_table = Gif.ColorTable(self._io, self, self._root)

        self.blocks = []
        i = 0
        while True:
            _ = Gif.Block(self._io, self, self._root)
            self.blocks.append(_)
            if _.type == 59:
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
            self.packed_fields = self._io.read_u1()
            if ((self.packed_fields >> 7) & 1) == 1:
                self.local_color_table = Gif.ColorTable(self._io, self, self._root)

            self.image_data = Gif.ImageData(self._io, self, self._root)

        @property
        def local_color_table_flag(self):
            if hasattr(self, '_m_local_color_table_flag'):
                return self._m_local_color_table_flag

            self._m_local_color_table_flag = ((self.packed_fields >> 7) & 1)
            return getattr(self, '_m_local_color_table_flag', None)

        @property
        def interlace_flag(self):
            if hasattr(self, '_m_interlace_flag'):
                return self._m_interlace_flag

            self._m_interlace_flag = ((self.packed_fields >> 6) & 1)
            return getattr(self, '_m_interlace_flag', None)

        @property
        def sort_flag(self):
            if hasattr(self, '_m_sort_flag'):
                return self._m_sort_flag

            self._m_sort_flag = ((self.packed_fields >> 5) & 1)
            return getattr(self, '_m_sort_flag', None)

        @property
        def local_color_table_size(self):
            if hasattr(self, '_m_local_color_table_size'):
                return self._m_local_color_table_size

            self._m_local_color_table_size = (1 << ((self.packed_fields & 7) + 1))
            return getattr(self, '_m_local_color_table_size', None)


    class BlockData(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.size = self._io.read_u1()
            self.data = self._io.read_bytes(self.size)


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
                _ = Gif.BlockData(self._io, self, self._root)
                self.blocks.append(_)
                if _.size == 0:
                    break
                i += 1


    class ColorTableEntry(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u1()
            self.green = self._io.read_u1()
            self.blue = self._io.read_u1()


    class LogicalScreenDescriptor(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.width = self._io.read_u2le()
            self.height = self._io.read_u2le()
            self.packed_fields = self._io.read_u1()
            self.background_color_index = self._io.read_u1()
            self.pixel_aspect_ratio = self._io.read_u1()

        @property
        def global_color_table_flag(self):
            if hasattr(self, '_m_global_color_table_flag'):
                return self._m_global_color_table_flag

            self._m_global_color_table_flag = ((self.packed_fields >> 7) & 1)
            return getattr(self, '_m_global_color_table_flag', None)

        @property
        def color_resolution(self):
            if hasattr(self, '_m_color_resolution'):
                return self._m_color_resolution

            self._m_color_resolution = (((self.packed_fields >> 4) & 7) + 1)
            return getattr(self, '_m_color_resolution', None)

        @property
        def sort_flag(self):
            if hasattr(self, '_m_sort_flag'):
                return self._m_sort_flag

            self._m_sort_flag = ((self.packed_fields >> 3) & 1)
            return getattr(self, '_m_sort_flag', None)

        @property
        def global_color_table_size(self):
            if hasattr(self, '_m_global_color_table_size'):
                return self._m_global_color_table_size

            self._m_global_color_table_size = (1 << ((self.packed_fields & 7) + 1))
            return getattr(self, '_m_global_color_table_size', None)


    class Block(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.type = self._io.read_u1()
            _on = self.type
            if _on == 33:
                self.content = Gif.Extension(self._io, self, self._root)
            elif _on == 44:
                self.content = Gif.ImageDescriptor(self._io, self, self._root)
            elif _on == 59:
                self.content = Gif.Trailer(self._io, self, self._root)


    class Trailer(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class ColorTable(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            for i in range(self._root.logical_screen_descriptor.global_color_table_size):
                self.entries.append(Gif.ColorTableEntry(self._io, self, self._root))



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
            self.version = self._io.read_bytes(3)


    class Extension(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.label = self._io.read_u1()
            if self.label != 254:
                self.block_size = self._io.read_u1()

            self.data = []
            i = 0
            while True:
                _ = Gif.BlockData(self._io, self, self._root)
                self.data.append(_)
                if _.size == 0:
                    break
                i += 1



