---
meta:
  id: "ntp4"
  version: "1.0.0"
  endian: big

fields:
  # Leap Indicator (LI)
  leap_indicator:
    type: uint8
    size: 2
    bitrange: [6, 7]

  # Version (VN)
  version_number:
    type: uint8
    size: 2
    bitrange: [3, 4]

  # Mode (MODE)
  mode:
    type: uint8
    size: 2
    bitrange: [0, 1]

  # Stratum (STRATUM)
  stratum:
    type: uint8
    size: 1

  # Poll Interval (POLL)
  poll_interval:
    type: int8
    size: 1

  # Precision (PREC)
  precision:
    type: int8
    size: 1

  # Root Delay (ROOTDELAY)
    root_delay:
      type: int32
      size: 4

  # Root Dispersion (ROOTDISPERSION)
    root_dispersion:
      type: uint32
      size: 4

  # Reference ID (REFID)
    ref_id:
      type: bytes
      size: 4

  # Reference Timestamp (REFTS)
    ref_timestamp:
      type: float_le
      size: 8

  # Originate Timestamp (ORIGTS)
    originate_timestamp:
      type: float_le
      size: 8

  # Receive Timestamp (RECVTS)
    receive_timestamp:
      type: float_le
      size: 8

  # Transmit Timestamp (TRNSMT)
    transmit_timestamp:
      type: float_le
      size: 8
---