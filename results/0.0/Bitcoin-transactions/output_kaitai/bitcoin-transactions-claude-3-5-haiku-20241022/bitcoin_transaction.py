# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class BitcoinTransaction(KaitaiStruct):
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
            self.inputs.append(BitcoinTransaction.TransactionInput(self._io, self, self._root))

        self.output_count = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
        self.outputs = []
        for i in range(self.output_count.value):
            self.outputs.append(BitcoinTransaction.TransactionOutput(self._io, self, self._root))

        self.locktime = self._io.read_u4le()

    class TransactionInput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.prev_tx_hash = self._io.read_bytes(32)
            self.prev_output_index = self._io.read_u4le()
            self.script_sig_length = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
            self.script_sig = self._io.read_bytes(self.script_sig_length.value)
            self.sequence_number = self._io.read_u4le()


    class TransactionOutput(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.amount = self._io.read_u8le()
            self.script_pubkey_length = BitcoinTransaction.VlqBase128Le(self._io, self, self._root)
            self.script_pubkey = self._io.read_bytes(self.script_pubkey_length.value)


    class VlqBase128Le(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.groups = []
            i = 0
            while not self._io.is_eof():
                self.groups.append(BitcoinTransaction.VlqBase128Le.Group(self._io, self, self._root))
                i += 1


        class Group(KaitaiStruct):
            def __init__(self, _io, _parent=None, _root=None):
                self._io = _io
                self._parent = _parent
                self._root = _root if _root else self
                self._read()

            def _read(self):
                self.byte = self._io.read_u1()

            @property
            def has_next(self):
                if hasattr(self, '_m_has_next'):
                    return self._m_has_next

                self._m_has_next = (self.byte & 128) != 0
                return getattr(self, '_m_has_next', None)

            @property
            def value(self):
                if hasattr(self, '_m_value'):
                    return self._m_value

                self._m_value = (self.byte & 127)
                return getattr(self, '_m_value', None)


        @property
        def value(self):
            if hasattr(self, '_m_value'):
                return self._m_value

            self._m_value = ((((self.groups[0].value + ((self.groups[1].value << 7) if len(self.groups) > 1 else 0)) + ((self.groups[2].value << 14) if len(self.groups) > 2 else 0)) + ((self.groups[3].value << 21) if len(self.groups) > 3 else 0)) if len(self.groups) > 0 else 0)
            return getattr(self, '_m_value', None)



