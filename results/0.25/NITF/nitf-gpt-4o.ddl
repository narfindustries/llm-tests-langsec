module NITF;

import "base" as base;

struct NITF {
    file_header: FileHeader;
    image_segments: [ImageSegment] @ file_header.image_segment_count;
    graphic_segments: [GraphicSegment] @ file_header.graphic_segment_count;
    text_segments: [TextSegment] @ file_header.text_segment_count;
    data_extension_segments: [DataExtensionSegment] @ file_header.data_extension_segment_count;
    reserved_extension_segments: [ReservedExtensionSegment] @ file_header.reserved_extension_segment_count;
}

struct FileHeader {
    file_type: string(4);
    version: string(5);
    complexity_level: string(2);
    system_type: string(2);
    originating_station_id: string(10);
    file_date_time: string(14);
    file_title: string(80);
    file_security: Security;
    file_copy_number: string(5);
    file_number_of_copies: string(5);
    encryption: Encryption;
    image_segment_count: uint16;
    graphic_segment_count: uint16;
    text_segment_count: uint16;
    data_extension_segment_count: uint16;
    reserved_extension_segment_count: uint16;
}

struct Security {
    classification: string(1);
    classification_system: string(2);
    codewords: string(11);
    control_and_handling: string(2);
    releasing_instructions: string(20);
    declassification_type: string(2);
    declassification_date: string(8);
    declassification_exemption: string(4);
    downgrade: string(1);
    downgrade_date: string(8);
}

struct Encryption {
    encryption: string(1);
    encryption_key: string(24);
}

struct ImageSegment {
    header: ImageSegmentHeader;
    data: bytes @ header.data_size;
}

struct ImageSegmentHeader {
    image_id: string(10);
    image_date_time: string(14);
    target_id: string(17);
    image_source: string(42);
    image_security: Security;
    image_mode: string(8);
    image_category: string(8);
    image_representation: string(4);
    image_coordinates: string(60);
    data_size: uint32;
}

struct GraphicSegment {
    header: GraphicSegmentHeader;
    data: bytes @ header.data_size;
}

struct GraphicSegmentHeader {
    graphic_id: string(10);
    graphic_date_time: string(14);
    graphic_source: string(42);
    graphic_security: Security;
    data_size: uint32;
}

struct TextSegment {
    header: TextSegmentHeader;
    data: bytes @ header.data_size;
}

struct TextSegmentHeader {
    text_id: string(10);
    text_date_time: string(14);
    text_security: Security;
    data_size: uint32;
}

struct DataExtensionSegment {
    header: DataExtensionSegmentHeader;
    data: bytes @ header.data_size;
}

struct DataExtensionSegmentHeader {
    data_extension_id: string(10);
    data_extension_date_time: string(14);
    data_extension_security: Security;
    data_size: uint32;
}

struct ReservedExtensionSegment {
    header: ReservedExtensionSegmentHeader;
    data: bytes @ header.data_size;
}

struct ReservedExtensionSegmentHeader {
    reserved_extension_id: string(10);
    reserved_extension_date_time: string(14);
    reserved_extension_security: Security;
    data_size: uint32;
}