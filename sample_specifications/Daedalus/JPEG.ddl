def Guard B = B is true

-- Start of image
def SOI = block
  Match [0xff, 0xd8]

-- End of image
def EOI = block
  Match [0xff, 0xd9]

-- JFIF metadata
def JFIF = block
  Match [0xff, 0xe0]
  len = Many 2 UInt8
  Match "JFIF"
  body = SegmentBody

-- EXIF metadata
def EXIF = block
  Match [0xff, 0xe1]
  len = Many 2 UInt8
  Match "Exif"
  body = SegmentBody

-- Match some segment
def Segment = block
  Many (1..) {$[0xff]}
  @tdat = UInt8
  tdat == 0xd9 is false  -- Not end of image

  -- tdat == 0xff is false  -- Not more padding
  -- tdat == 0x00 is false  -- Not an escaped byte
  First
    sof0  = TaggedBody 0xc0 tdat
    sof1  = TaggedBody 0xc1 tdat
    sof2  = TaggedBody 0xc2 tdat
    sof3  = TaggedBody 0xc3 tdat
    sof4  = TaggedBody 0xc4 tdat
    sof5  = TaggedBody 0xc5 tdat
    sof6  = TaggedBody 0xc6 tdat
    sof7  = TaggedBody 0xc7 tdat
    sof8  = TaggedBody 0xc8 tdat
    sof9  = TaggedBody 0xc9 tdat
    sof10 = TaggedBody 0xcA tdat
    sof11 = TaggedBody 0xcB tdat
    sof13 = TaggedBody 0xcD tdat
    sof14 = TaggedBody 0xcE tdat
    sof15 = TaggedBody 0xcF tdat

    dht   = TaggedBody 0xc4 tdat
    dac   = TaggedBody 0xcc tdat
    sos   = TaggedBody 0xda tdat
    dqt   = TaggedBody 0xdb tdat
    dnl   = TaggedBody 0xdc tdat
    dri   = TaggedBody 0xdd tdat
    dhp   = TaggedBody 0xde tdat
    exp   = TaggedBody 0xdf tdat

    com   = TaggedBody 0xfe tdat

    other = { tag = ^tdat; body = SegmentBody }


def TaggedBody marker tdat = block
  Guard (marker == tdat)
  tag = tdat
  body = SegmentBody

def SegmentBody = Many {
  First
    { @i = UInt8; i == 0xff is false; ^ i }
    { Match [0xff, 0x00]; ^ 0xff }
}

def Main = block
  SOI
  jfif = JFIF
  exif = EXIF
  segments = Many Segment
  EOI
  END