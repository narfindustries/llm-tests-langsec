module JPEG {

  type WORD = U16be;
  type DWORD = U32be;

  type JPEGFile = {
    segments: Segment[]
  };

  type Segment = {
    marker: WORD,
    len:    U16be,
    data:   U8[len - 2]
  } | {
    marker: 0xFFDA, // Start Of Scan (SOS)
    len:    U16be,
    data:   ScanData
  };

  type ScanData = {
    components: ComponentSpec[],
    image_data: U8[]
  };

  type ComponentSpec = {
    id:      U8,
    hfactor: U4,
    vfactor: U4,
    qtable:  U8
  };

  let main = parse JPEGFile;
}