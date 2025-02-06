grammar png;

file = header chunk+;

header = magic;

magic = "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A";

chunk = length type data crc;
length = u32;
type = id4;
data = byte[length];
crc = u32;

ihdr = width height bit_depth color_type compression filter interlace;
width = u32;
height = u32;
bit_depth = u8 where value in [1,2,4,8,16];
color_type = u8 where value in [0,2,3,4,6];
compression = u8 where value == 0;
filter = u8 where value == 0;
interlace = u8 where value in [0,1];

plte = rgb+;
rgb = r:u8 g:u8 b:u8;

trns = grayscale | rgb_trns | palette_alpha;
grayscale = gray:u16;
rgb_trns = r:u16 g:u16 b:u16;
palette_alpha = alpha:u8[];

chrm = white_x:u32 white_y:u32 red_x:u32 red_y:u32 green_x:u32 green_y:u32 blue_x:u32 blue_y:u32;

gama = gamma:u32;

iccp = name compression profile;
name = str terminated 0x00 length 1..79;
profile = byte[];

sbit = gray_bits | rgb_bits | gray_alpha_bits | rgba_bits;
gray_bits = gray:u8;
rgb_bits = red:u8 green:u8 blue:u8;
gray_alpha_bits = gray:u8 alpha:u8;
rgba_bits = red:u8 green:u8 blue:u8 alpha:u8;

srgb = intent:u8 where value in [0,1,2,3];

text = keyword text_data;
keyword = str terminated 0x00 length 1..79;
text_data = str;

ztxt = keyword compression compressed_text;
compressed_text = byte[];

itxt = keyword compression_flag compression_method language translated_keyword text_string;
language = str terminated 0x00;
translated_keyword = str terminated 0x00;
text_string = str;

bkgd = gray_bg | rgb_bg | palette_bg;
gray_bg = gray:u16;
rgb_bg = red:u16 green:u16 blue:u16;
palette_bg = index:u8;

hist = frequencies:u16[];

phys = x:u32 y:u32 unit:u8 where value in [0,1];

time = year:u16 month:u8 day:u8 hour:u8 minute:u8 second:u8;

splt = name depth palette_entries;
depth = u8 where value in [8,16];
palette_entries = byte[];

id4 = char[4];