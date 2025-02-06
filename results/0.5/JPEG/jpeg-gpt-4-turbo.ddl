type JPEG = struct {
    segments: [Segment];
};

type Segment = union {
    0xFFD8 -> SOI: void;
    0xFFD9 -> EOI: void;
    0xFFC0 -> SOF: SOF;
    0xFFDA -> SOS: SOS;
    0xFFDB -> DQT: DQT;
    0xFFC4 -> DHT: DHT;
    0xFFFE -> COM: COM;
    [0xFFE0..0xFFEF] -> APPn: APPn;
};

type SOF = struct {
    length: u16;
    precision: u8;
    height: u16;
    width: u16;
    num_components: u8;
    components: [SOFComponent](:num_components);
};

type SOFComponent = struct {
    component_id: u8;
    sampling_factors: u8;
    quant_table_id: u8;
};

type SOS = struct {
    length: u16;
    num_components: u8;
    components: [SOSComponent](:num_components);
    start_spectral_selection: u8;
    end_spectral_selection: u8;
    successive_approximation: u8;
};

type SOSComponent = struct {
    component_id: u8;
    huffman_table_ids: u8;
};

type DQT = struct {
    length: u16;
    tables: [DQTTable];
};

type DQTTable = struct {
    table_info: u8;
    values: [u8](64);
};

type DHT = struct {
    length: u16;
    tables: [DHTTable];
};

type DHTTable = struct {
    table_info: u8;
    num_codes: [u16](16);
    symbols: [u8];
};

type COM = struct {
    length: u16;
    comment: [u8];
};

type APPn = struct {
    length: u16;
    data: [u8];
};