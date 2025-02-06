def file GIF_file {
  struct GIF_header {
    bytes Signature : size => 3;
    bytes Version : size => 3;
  }
  struct GIF_lsd {
    uint16 Canvas_Width;
    uint16 Canvas_Height;
    byte Packed_Fields;
    byte Background_Color_Index;
    byte Pixel_Aspect_Ratio;
    if ((Packed_Fields >> 7) & 1) { // Global Color Table Flag
      list Global_Color_Table : size => (1 << ((Packed_Fields & 0x07) + 1)) of struct RGB_color {
        byte Red;
        byte Green;
        byte Blue;
      }
    }
  }
  list Data : many of struct GIF_data_block {
    either {
      struct {
        byte Image_Separator : const => 0x2C;
        uint16 Image_Left_Position;
        uint16 Image_Top_Position;
        uint16 Image_Width;
        uint16 Image_Height;
        byte Packed_Fields;
        if ((Packed_Fields >> 7) & 1) { // Local Color Table Flag
          list Local_Color_Table : size => (1 << ((Packed_Fields & 0x07) + 1)) of struct RGB_color {
            byte Red;
            byte Green;
            byte Blue;
          }
        }
        struct GIF_image_data {
          byte LZW_Minimum_Code_Size;
          list Image_Data_Blocks : many of struct GIF_data_sub_block {
            byte Block_Size;
            bytes Data : size => Block_Size;
          }
        }
      }
      struct {
        byte Extension_Introducer : const => 0x21;
        byte Extension_Label;
        switch (Extension_Label) {
          case 0xF9: // Graphic Control Extension
            struct GIF_gce {
              byte Block_Size : const => 0x04;
              byte Packed_Fields;
              uint16 Delay_Time;
              byte Transparent_Color_Index;
              byte Block_Terminator : const => 0x00;
            }
          case 0xFE: // Comment Extension
            struct GIF_comment {
              list Comment_Data_Blocks : many of struct GIF_data_sub_block {
                byte Block_Size;
                bytes Data : size => Block_Size;
              }
            }
          case 0xFF: // Application Extension
            struct GIF_application {
              byte Block_Size : const => 0x0B;
              bytes Application_Identifier : size => 8;
              bytes Application_Authentication_Code : size => 3;
              list Application_Data_Blocks : many of struct GIF_data_sub_block {
                byte Block_Size;
                bytes Data : size => Block_Size;
              }
            }
          case 0x01: // Plain Text Extension
            struct GIF_plain_text {
              byte Block_Size : const => 0x12;
              uint16 Text_Grid_Left_Position;
              uint16 Text_Grid_Top_Position;
              uint16 Text_Grid_Width;
              uint16 Text_Grid_Height;
              byte Character_Cell_Width;
              byte Character_Cell_Height;
              byte Text_Foreground_Color_Index;
              byte Text_Background_Color_Index;
              list Plain_Text_Data : many of struct GIF_data_sub_block {
                byte Block_Size;
                bytes Data : size => Block_Size;
              }
            }
        }
      }
    }
  }
  byte Trailer : const => 0x3B;
}