def JPEG = {
  let soi = magic16 0xFFD8;
  let app_segments = many (
    let marker = magic16 0xFFE0;
    let length = u16;
    let identifier = string (length - 2);
    let version = u16;
    let density_units = u8;
    let x_density = u16;
    let y_density = u16;
    { marker, length, identifier, version, density_units, x_density, y_density }
  );
  
  let quantization_tables = many (
    let marker = magic16 0xFFDB;
    let length = u16;
    let precision = u8;
    let table_id = bits 4;
    let table_values = array 64 (if precision == 8 then u8 else u16);
    { marker, length, precision, table_id, table_values }
  );
  
  let start_of_frame = (
    magic16 0xFFC0
  ) >> {
    let length = u16;
    let precision = u8;
    let height = u16;
    let width = u16;
    let components = u8;
    let component_details = array components {
      let id = u8;
      let sampling_factors = u8;
      let quantization_table = u8;
    };
    { length, precision, height, width, components, component_details }
  };
  
  let huffman_tables = many (
    let marker = magic16 0xFFC4;
    let length = u16;
    let table_class = u8;
    let table_destination = bits 4;
    let code_lengths = array 16 u8;
    let values = array (sum code_lengths) u8;
    { marker, length, table_class, table_destination, code_lengths, values }
  );
  
  let restart_interval = optional (
    let marker = magic16 0xFFDD;
    let length = u16;
    let interval = u16;
    { marker, length, interval }
  );
  
  let scan_components = {
    let marker = magic16 0xFFC0;
    let length = u16;
    let components = u8;
    let component_details = array components {
      let selector = u8;
      let huffman_tables = u8;
    };
    let spectral_start = u8;
    let spectral_end = u8;
    let approximation = u8;
    { marker, length, components, component_details, spectral_start, spectral_end, approximation }
  };
  
  let restart_markers = many (
    magic16 0xFFD0
  );
  
  let compressed_data = remaining_bytes;
  
  let eoi = magic16 0xFFD9;
  
  { 
    soi, 
    app_segments, 
    quantization_tables, 
    start_of_frame, 
    huffman_tables, 
    restart_interval, 
    scan_components, 
    restart_markers, 
    compressed_data, 
    eoi 
  }
}