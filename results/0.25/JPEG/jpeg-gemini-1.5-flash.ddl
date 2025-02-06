data JPEG = SOI { soi : Unit }
          | APPn { appn_marker : Byte, appn_length : UInt16, appn_data : [Byte] }
          | DQT { dqt_marker : Byte, dqt_length : UInt16, dqt : QuantizationTable }
          | DHT { dht_marker : Byte, dht_length : UInt16, dht : HuffmanTable }
          | SOF { sof_marker : Byte, sof_length : UInt16, sof : StartOfFrame }
          | ScanData { scan_marker : Byte, scan_length : UInt16, scan_data : [Byte] }
          | EOI { eoi_marker : Byte, eoi : Unit }

data QuantizationTable = QuantizationTable { precision : Byte, table_data : [Int16] }

data HuffmanTable = HuffmanTable { table_class : Byte, table_id : Byte, code_lengths : [Int], symbols : [Byte] }

data StartOfFrame = StartOfFrame { precision : Byte, height : UInt16, width : UInt16, number_of_components : Byte, components : [Component] }

data Component = Component { component_id : Byte, horizontal_sampling_factor : Byte, vertical_sampling_factor : Byte, quantization_table_selector : Byte }

-- Note: This is still a highly simplified representation.  The actual JPEG standard is far more complex.
-- Many fields are omitted or highly abstracted for brevity. Error handling and detailed data structures
-- (like Huffman codes and DCT coefficients) are not included. This is only a skeletal representation.
-- A complete Daedalus specification would be extremely large and complex.
