format png {
  magic: byte[8] = [137, 80, 78, 71, 13, 10, 26, 10];

  struct Chunk {
    length: uintbe32;
    type: byte[4];
    data: byte[length];
    crc: uintbe32;
  }

  struct IHDR {
    width: uintbe32;
    height: uintbe32;
    bit_depth: byte;
    color_type: byte;
    compression_method: byte = 0;
    filter_method: byte = 0;
    interlace_method: byte;
    constraint bit_depth in [1, 2, 4, 8, 16];
    constraint color_type in [0, 2, 3, 4, 6];
    constraint interlace_method in [0, 1];
  }

  struct PLTE {
    entries: struct {
      red: byte;
      green: byte;
      blue: byte;
    }[1, 256];
  }

  struct IDAT {
    data: byte[1, infinity];
  }

  struct IEND {
  }

  struct TRNS {
    gray: uintbe16[0, 256];
    red: byte[0, 256];
    green: byte[0, 256];
    blue: byte[0, 256];
    alpha: byte[0, 256];
  }

  struct Ancillary {
    length: uintbe32;
    type: byte[4];
    data: byte[length];
    crc: uintbe32;
  }

  main = magic
    ~ ihdr: Chunk {
      constraint ihdr.type == "IHDR";
      constraint ihdr.length == 13;
      constraint ihdr.data == IHDR;
    }
    ~ plt: PLTE[0, 1] {
      constraint plt.type == "PLTE";
      constraint plt.length % 3 == 0;
    }
    ~ idat: IDAT[1, infinity] {
      constraint idat.type == "IDAT";
    }
    ~ trns: TRNS[0, 1] {
      constraint trns.type == "tRNS";
    }
    ~ anc: Ancillary[0, infinity]
    ~ iend: Chunk {
      constraint iend.type == "IEND";
      constraint iend.length == 0;
    }
}