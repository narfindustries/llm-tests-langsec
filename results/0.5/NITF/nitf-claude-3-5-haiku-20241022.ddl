type NITF_version = enum {
    Version_2_0,
    Version_2_1
}

type FileHeader = struct {
    file_profile_name: string(9),
    complexity_level: uint8,
    standard_type: string(2),
    originating_station: string(10),
    file_title: string(80),
    security_classification: char,
    security_code: string(2),
    file_copy_number: string(5),
    file_number_of_copies: string(5),
    encryption: char,
    file_background_color: string(3),
    version: NITF_version
}

type ImageSegment = struct {
    image_header_length: uint32,
    image_data_length: uint32,
    image_data: bytes
}

type NITFFile = struct {
    header: FileHeader,
    image_segments: list(ImageSegment)
}

parser nitf_parser = {
    parse_file_header: func(input: input_stream) -> FileHeader {
        let file_profile_name = read_string(input, 9);
        let complexity_level = read_uint8(input);
        let standard_type = read_string(input, 2);
        let originating_station = read_string(input, 10);
        let file_title = read_string(input, 80);
        let security_classification = read_char(input);
        let security_code = read_string(input, 2);
        let file_copy_number = read_string(input, 5);
        let file_number_of_copies = read_string(input, 5);
        let encryption = read_char(input);
        let file_background_color = read_string(input, 3);
        
        let version = match (standard_type) {
            "NM" => NITF_version.Version_2_0,
            "NS" => NITF_version.Version_2_1,
            _ => error("Unknown NITF version")
        };

        return FileHeader {
            file_profile_name: file_profile_name,
            complexity_level: complexity_level,
            standard_type: standard_type,
            originating_station: originating_station,
            file_title: file_title,
            security_classification: security_classification,
            security_code: security_code,
            file_copy_number: file_copy_number,
            file_number_of_copies: file_number_of_copies,
            encryption: encryption,
            file_background_color: file_background_color,
            version: version
        }
    },

    parse_image_segment: func(input: input_stream) -> ImageSegment {
        let image_header_length = read_uint32(input);
        let image_data_length = read_uint32(input);
        let image_data = read_bytes(input, image_data_length);

        return ImageSegment {
            image_header_length: image_header_length,
            image_data_length: image_data_length,
            image_data: image_data
        }
    },

    parse: func(input: input_stream) -> NITFFile {
        let header = parse_file_header(input);
        let image_segments = [];

        while (!is_eof(input)) {
            let image_segment = parse_image_segment(input);
            image_segments.append(image_segment);
        }

        return NITFFile {
            header: header,
            image_segments: image_segments
        }
    }
}