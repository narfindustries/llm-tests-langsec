# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class BitcoinTransaction(KaitaiStruct):
    """Structure of a Bitcoin transaction
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.version = self._io.read_u4le()
        self.input_count = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
        self.inputs = []
        for i in range(self.input_count.value):
            self.inputs.append(BitcoinTransaction.Input(self._io, self, self._root))

        self.output_count = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.value):
            self.outputs.append(BitcoinTransaction.Output(self._io, self, self._root))

        self.locktime = self._io.read_u4le()

    class Input(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prev_tx_hash = []
            for i in range(4):
                self.prev_tx_hash.append(self._io.read_u8le())

            self.output_index = self._io.read_u4le()
            self.script_length = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_length.value)
            self.sequence = self._io.read_u4le()


    class Output(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.value = self._io.read_u8le()
            self.script_length = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
            self.script_pub_key = self._io.read_bytes(self.script_length.value)


    class VlqBase128Le(KaitaiStruct):
        """Variable-length quantity (VLQ) in little-endian form, used to encode integer values for better compression. Commonly used in Bitcoin's protocol."""
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups = []
            i = 0
            while True:
                _ = self._io.read_u1()
                self.groups.append(_)
                if (_ & 128) == 0:
                    break
                i += 1

        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((((((self.groups[0] & 127) if len(self.groups) > 0 else 0) + (((self.groups[1] & 127) << 7) if len(self.groups) > 1 else 0)) + (((self.groups[2] & 127) << 14) if len(self.groups) > 2 else 0)) + (((self.groups[3] & 127) << 21) if len(self.groups) > 3 else 0)) + (((self.groups[4] & 127) << 28) if len(self.groups) > 4 else 0))
            return getattr(self, '_m_value', None)



