format ARP {
  HTYPE: uint16 {
    1: "Ethernet (10Mb)",
    2: "Experimental Ethernet (3Mb)",
    3: "Amateur Radio AX.25",
    4: "Proteon ProNET Token Ring",
    5: "Chaos",
    6: "IEEE 802 Networks",
    7: "ARCNET",
    8: "Hyperchannel",
    9: "Lanstar",
    10: "Autonet Short Address",
    11: "LocalTalk",
    12: "LocalNet",
    13: "Ultra Link",
    14: "SMDS",
    15: "Frame Relay",
    16: "ATM",
    17: "HDLC",
    18: "Fibre Channel",
    19: "ATMARP",
    20: "Serial Line",
    21: "ATM",
    22: "MIL-STD-188-220",
    23: "Metricom",
    24: "IEEE 1394.1995",
    25: "MAPOS",
    26: "Twinaxial",
    27: "EUI-64",
    28: "HIPARP",
    29: "IP and ARP over ISO 802.11",
    30: "ARCNET (new)",
    31: "Cambridge Ring"
  },
  PTYPE: uint16 {
    0x0800: "IPv4",
    0x0806: "ARP",
    0x8035: "Reverse ARP"
  },
  OPER: uint16 {
    1: "Request",
    2: "Reply",
    3: "Request Reverse",
    4: "Reply Reverse"
  },
  header: [
    { name: "htype", type: "uint16" },
    { name: "ptype", type: "uint16" },
    { name: "hlen", type: "uint8" },
    { name: "plen", type: "uint8" },
    { name: "oper", type: "uint16" }
  ],
  body: [
    { name: "sha", type: "bytes", length: "hlen" },
    { name: "spa", type: "bytes", length: "plen" },
    { name: "tha", type: "bytes", length: "hlen" },
    { name: "tpa", type: "bytes", length: "plen" }
  ]
}