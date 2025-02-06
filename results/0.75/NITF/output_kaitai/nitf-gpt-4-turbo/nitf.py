# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class Nitf(KaitaiStruct):
    """The NITF (National Imagery Transmission Format) standard is designed to
    handle formatted image and associated textual and other data. NITF is
    used by the US government and military for transmitting digital images.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = Nitf.FileHeader(self._io, self, self._root)
        self.image_segments = []
        for i in range(self.header.numi):
            self.image_segments.append(Nitf.ImageSegment(self._io, self, self._root))

        self.graphics_segments = []
        for i in range(self.header.numg):
            self.graphics_segments.append(Nitf.GraphicsSegment(self._io, self, self._root))

        self.text_segments = []
        for i in range(self.header.numt):
            self.text_segments.append(Nitf.TextSegment(self._io, self, self._root))

        self.data_extension_segments = []
        for i in range(self.header.numdes):
            self.data_extension_segments.append(Nitf.DataExtensionSegment(self._io, self, self._root))

        self.reserved_extension_segments = []
        for i in range(self.header.numres):
            self.reserved_extension_segments.append(Nitf.ReservedExtensionSegment(self._io, self, self._root))


    class ReservedExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_subheader = Nitf.ResSubheader(self._io, self, self._root)
            self.res_data_field = self._io.read_bytes_full()


    class LengthInfo(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.length_image_segment = self._io.read_u4be()


    class TextSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.textid = (self._io.read_bytes(7)).decode(u"ASCII")
            self.txtalvl = self._io.read_u2be()
            self.txtdate = (self._io.read_bytes(14)).decode(u"ASCII")
            self.txttitl = (self._io.read_bytes(80)).decode(u"ASCII")
            self.txtcls = (self._io.read_bytes(1)).decode(u"ASCII")


    class ImageSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.image_subheader = Nitf.ImageSubheader(self._io, self, self._root)
            self.image_data_field = self._io.read_bytes_full()


    class TextSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.text_subheader = Nitf.TextSubheader(self._io, self, self._root)
            self.text_data_field = self._io.read_bytes_full()


    class GraphicSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.sid = (self._io.read_bytes(10)).decode(u"ASCII")
            self.sname = (self._io.read_bytes(20)).decode(u"ASCII")
            self.sclass = (self._io.read_bytes(1)).decode(u"ASCII")


    class GraphicsSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.graphic_subheader = Nitf.GraphicSubheader(self._io, self, self._root)
            self.graphic_data_field = self._io.read_bytes_full()


    class DataExtensionSegment(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.des_subheader = Nitf.DesSubheader(self._io, self, self._root)
            self.des_data_field = self._io.read_bytes_full()


    class ImageSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.iid1 = (self._io.read_bytes(10)).decode(u"ASCII")
            self.idatim = (self._io.read_bytes(14)).decode(u"ASCII")
            self.tgtid = (self._io.read_bytes(17)).decode(u"ASCII")
            self.iid2 = (self._io.read_bytes(80)).decode(u"ASCII")
            self.isclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.isclas_sys = (self._io.read_bytes(2)).decode(u"ASCII")
            self.iscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.isctlh = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isrel = (self._io.read_bytes(20)).decode(u"ASCII")
            self.isdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.isdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.isdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.isdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.iscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.iscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.iscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.iscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.issrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.isctln = (self._io.read_bytes(15)).decode(u"ASCII")
            self.isorce = (self._io.read_bytes(42)).decode(u"ASCII")


    class ResSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.res_type = (self._io.read_bytes(25)).decode(u"ASCII")
            self.res_version = self._io.read_u1()
            self.ressclas = (self._io.read_bytes(1)).decode(u"ASCII")


    class DesSubheader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.desid = (self._io.read_bytes(25)).decode(u"ASCII")
            self.desver = self._io.read_u1()
            self.dessclas = (self._io.read_bytes(1)).decode(u"ASCII")


    class FileHeader(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.file_profile_name = (self._io.read_bytes(4)).decode(u"ASCII")
            self.file_version = (self._io.read_bytes(5)).decode(u"ASCII")
            self.clevel = (self._io.read_bytes(2)).decode(u"ASCII")
            self.stype = (self._io.read_bytes(4)).decode(u"ASCII")
            self.osta_id = (self._io.read_bytes(10)).decode(u"ASCII")
            self.fdt = (self._io.read_bytes(14)).decode(u"ASCII")
            self.ftitle = (self._io.read_bytes(80)).decode(u"ASCII")
            self.fsclas = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fsclas_sys = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fscode = (self._io.read_bytes(11)).decode(u"ASCII")
            self.fsctlh = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsrel = (self._io.read_bytes(20)).decode(u"ASCII")
            self.fsdctp = (self._io.read_bytes(2)).decode(u"ASCII")
            self.fsdcdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsdcxm = (self._io.read_bytes(4)).decode(u"ASCII")
            self.fsdg = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fsdgdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fscltx = (self._io.read_bytes(43)).decode(u"ASCII")
            self.fscatp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fscaut = (self._io.read_bytes(40)).decode(u"ASCII")
            self.fscrsn = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fssrdt = (self._io.read_bytes(8)).decode(u"ASCII")
            self.fsctln = (self._io.read_bytes(15)).decode(u"ASCII")
            self.fscop = (self._io.read_bytes(5)).decode(u"ASCII")
            self.fscpys = (self._io.read_bytes(5)).decode(u"ASCII")
            self.encryp = (self._io.read_bytes(1)).decode(u"ASCII")
            self.fbkgc = (self._io.read_bytes(3)).decode(u"ASCII")
            self.oname = (self._io.read_bytes(27)).decode(u"ASCII")
            self.ophone = (self._io.read_bytes(18)).decode(u"ASCII")
            self.numi = self._io.read_u2be()
            self.linfo = []
            for i in range(self.numi):
                self.linfo.append(Nitf.LengthInfo(self._io, self, self._root))

            self.numg = self._io.read_u1()
            self.numx = self._io.read_u1()
            self.numt = self._io.read_u1()
            self.numdes = self._io.read_u1()
            self.numres = self._io.read_u1()
            self.udidl = self._io.read_u4be()
            self.udid = self._io.read_bytes(self.udidl)
            self.xhdl = self._io.read_u4be()
            self.xhd = self._io.read_bytes(self.xhdl)



