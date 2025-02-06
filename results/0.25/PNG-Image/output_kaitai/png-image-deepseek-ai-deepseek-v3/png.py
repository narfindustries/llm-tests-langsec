# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Png(KaitaiStruct):
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.magic = self._io.read_bytes(9)
        if not self.magic == b"\xC2\x89\x50\x4E\x47\x0D\x0A\x1A\x0A":
            raise kaitaistruct.ValidationNotEqualError(b"\xC2\x89\x50\x4E\x47\x0D\x0A\x1A\x0A", self.magic, self._io, u"/seq/0")
        self.chunks = []
        i = 0
        while not self._io.is_eof():
            self.chunks.append(Png.Chunk(self._io, self, self._root))
            i += 1


    class Rgb(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.red = self._io.read_u1()
            self.green = self._io.read_u1()
            self.blue = self._io.read_u1()


    class ZtxtChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.compression_method = self._io.read_u1()
            self.compressed_text = self._io.read_bytes_full()


    class Chunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length = self._io.read_u4be()
            self.type = (self._io.read_bytes(4)).decode(u"ASCII")
            self.data = self._io.read_bytes(self.length)
            self.crc = self._io.read_u4be()

        @property
        def idat(self):
            if hasattr(self, '_m_idat'):
                return self._m_idat

            if self.type == u"IDAT":
                self._m_idat = Png.IdatChunk(self._io, self, self._root)

            return getattr(self, '_m_idat', None)

        @property
        def splt(self):
            if hasattr(self, '_m_splt'):
                return self._m_splt

            if self.type == u"sPLT":
                self._m_splt = Png.SpltChunk(self._io, self, self._root)

            return getattr(self, '_m_splt', None)

        @property
        def bkgd(self):
            if hasattr(self, '_m_bkgd'):
                return self._m_bkgd

            if self.type == u"bKGD":
                self._m_bkgd = Png.BkgdChunk(self._io, self, self._root)

            return getattr(self, '_m_bkgd', None)

        @property
        def trns(self):
            if hasattr(self, '_m_trns'):
                return self._m_trns

            if self.type == u"tRNS":
                self._m_trns = Png.TrnsChunk(self._io, self, self._root)

            return getattr(self, '_m_trns', None)

        @property
        def iend(self):
            if hasattr(self, '_m_iend'):
                return self._m_iend

            if self.type == u"IEND":
                self._m_iend = Png.IendChunk(self._io, self, self._root)

            return getattr(self, '_m_iend', None)

        @property
        def ihdr(self):
            if hasattr(self, '_m_ihdr'):
                return self._m_ihdr

            if self.type == u"IHDR":
                self._m_ihdr = Png.IhdrChunk(self._io, self, self._root)

            return getattr(self, '_m_ihdr', None)

        @property
        def ztxt(self):
            if hasattr(self, '_m_ztxt'):
                return self._m_ztxt

            if self.type == u"zTXt":
                self._m_ztxt = Png.ZtxtChunk(self._io, self, self._root)

            return getattr(self, '_m_ztxt', None)

        @property
        def iccp(self):
            if hasattr(self, '_m_iccp'):
                return self._m_iccp

            if self.type == u"iCCP":
                self._m_iccp = Png.IccpChunk(self._io, self, self._root)

            return getattr(self, '_m_iccp', None)

        @property
        def text(self):
            if hasattr(self, '_m_text'):
                return self._m_text

            if self.type == u"tEXt":
                self._m_text = Png.TextChunk(self._io, self, self._root)

            return getattr(self, '_m_text', None)

        @property
        def phys(self):
            if hasattr(self, '_m_phys'):
                return self._m_phys

            if self.type == u"pHYs":
                self._m_phys = Png.PhysChunk(self._io, self, self._root)

            return getattr(self, '_m_phys', None)

        @property
        def hist(self):
            if hasattr(self, '_m_hist'):
                return self._m_hist

            if self.type == u"hIST":
                self._m_hist = Png.HistChunk(self._io, self, self._root)

            return getattr(self, '_m_hist', None)

        @property
        def sbit(self):
            if hasattr(self, '_m_sbit'):
                return self._m_sbit

            if self.type == u"sBIT":
                self._m_sbit = Png.SbitChunk(self._io, self, self._root)

            return getattr(self, '_m_sbit', None)

        @property
        def chrm(self):
            if hasattr(self, '_m_chrm'):
                return self._m_chrm

            if self.type == u"cHRM":
                self._m_chrm = Png.ChrmChunk(self._io, self, self._root)

            return getattr(self, '_m_chrm', None)

        @property
        def plte(self):
            if hasattr(self, '_m_plte'):
                return self._m_plte

            if self.type == u"PLTE":
                self._m_plte = Png.PlteChunk(self._io, self, self._root)

            return getattr(self, '_m_plte', None)

        @property
        def gama(self):
            if hasattr(self, '_m_gama'):
                return self._m_gama

            if self.type == u"gAMA":
                self._m_gama = Png.GamaChunk(self._io, self, self._root)

            return getattr(self, '_m_gama', None)

        @property
        def time(self):
            if hasattr(self, '_m_time'):
                return self._m_time

            if self.type == u"tIME":
                self._m_time = Png.TimeChunk(self._io, self, self._root)

            return getattr(self, '_m_time', None)

        @property
        def srgb(self):
            if hasattr(self, '_m_srgb'):
                return self._m_srgb

            if self.type == u"sRGB":
                self._m_srgb = Png.SrgbChunk(self._io, self, self._root)

            return getattr(self, '_m_srgb', None)

        @property
        def itxt(self):
            if hasattr(self, '_m_itxt'):
                return self._m_itxt

            if self.type == u"iTXt":
                self._m_itxt = Png.ItxtChunk(self._io, self, self._root)

            return getattr(self, '_m_itxt', None)


    class IdatChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.compressed_data = self._io.read_bytes_full()


    class ChrmChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.white_point_x = self._io.read_u4be()
            self.white_point_y = self._io.read_u4be()
            self.red_x = self._io.read_u4be()
            self.red_y = self._io.read_u4be()
            self.green_x = self._io.read_u4be()
            self.green_y = self._io.read_u4be()
            self.blue_x = self._io.read_u4be()
            self.blue_y = self._io.read_u4be()


    class IendChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            pass


    class IhdrChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.width = self._io.read_u4be()
            self.height = self._io.read_u4be()
            self.bit_depth = self._io.read_u1()
            self.color_type = self._io.read_u1()
            self.compression_method = self._io.read_u1()
            self.filter_method = self._io.read_u1()
            self.interlace_method = self._io.read_u1()


    class PlteChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.entries = []
            for i in range(self._parent.length // 3):
                self.entries.append(Png.Rgb(self._io, self, self._root))



    class SrgbChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.rendering_intent = self._io.read_u1()


    class ItxtChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")
            self.compression_flag = self._io.read_u1()
            self.compression_method = self._io.read_u1()
            self.language_tag = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.translated_keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")
            self.text = (self._io.read_bytes_term(0, False, True, True)).decode(u"UTF-8")


    class GamaChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.gamma = self._io.read_u4be()


    class TrnsChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.transparency_data = self._io.read_bytes_full()


    class SbitChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.significant_bits = self._io.read_bytes_full()


    class BkgdChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.background_color = self._io.read_bytes_full()


    class PhysChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.pixels_per_unit_x = self._io.read_u4be()
            self.pixels_per_unit_y = self._io.read_u4be()
            self.unit_specifier = self._io.read_u1()


    class IccpChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.profile_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.compression_method = self._io.read_u1()
            self.compressed_profile = self._io.read_bytes_full()


    class HistChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.histogram = []
            for i in range(self._parent.length // 2):
                self.histogram.append(self._io.read_u2be())



    class SpltChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.palette_name = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.sample_depth = self._io.read_u1()
            self.palette_entries = self._io.read_bytes_full()


    class TextChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.keyword = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")
            self.text = (self._io.read_bytes_term(0, False, True, True)).decode(u"ASCII")


    class TimeChunk(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.year = self._io.read_u2be()
            self.month = self._io.read_u1()
            self.day = self._io.read_u1()
            self.hour = self._io.read_u1()
            self.minute = self._io.read_u1()
            self.second = self._io.read_u1()



