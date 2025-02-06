def NITF {
    type FileHeader {
        file_profile_name: String(9),
        file_version: String(5),
        complexity_level: UInt8,
        standard_type: String(2),
        originating_station_id: String(10),
        file_datetime: String(14),
        file_title: String(80),
        security_class: Enum(
            TopSecret = 0,
            Secret = 1,
            Confidential = 2,
            Restricted = 3,
            Unclassified = 4
        ),
        security_country_code: String(2),
        security_release_instructions: String(20)
    }

    type ImageSegment {
        image_subheader: ImageSubheader,
        image_data: Bytes
    }

    type ImageSubheader {
        image_identifier: String(10),
        image_datetime: String(14),
        image_target_id: String(17),
        image_source: String(42),
        number_of_rows: UInt32,
        number_of_columns: UInt32,
        pixel_value_type: Enum(
            Integer = 0,
            Real = 1,
            Complex = 2
        ),
        image_representation: Enum(
            Monochrome = 0,
            RGB = 1,
            YCbCr = 2
        ),
        image_category: Enum(
            Visual = 0,
            Infrared = 1,
            Radar = 2,
            Multispectral = 3
        )
    }

    type GraphicSegment {
        graphic_subheader: GraphicSubheader,
        graphic_data: Bytes
    }

    type GraphicSubheader {
        graphic_identifier: String(10),
        graphic_name: String(20),
        graphic_type: Enum(
            Bitmap = 0,
            Vector = 1
        )
    }

    type TextSegment {
        text_subheader: TextSubheader,
        text_data: Bytes
    }

    type TextSubheader {
        text_identifier: String(10),
        text_datetime: String(14),
        text_title: String(80)
    }

    type DataExtensionSegment {
        data_extension_subheader: DataExtensionSubheader,
        data_extension_data: Bytes
    }

    type DataExtensionSubheader {
        extension_type: String(2),
        extension_subtype: String(6)
    }

    type ReservedExtensionSegment {
        reserved_extension_subheader: ReservedExtensionSubheader,
        reserved_extension_data: Bytes
    }

    type ReservedExtensionSubheader {
        extension_type: String(2)
    }

    type NITFFile {
        file_header: FileHeader,
        image_segments: [ImageSegment],
        graphic_segments: [GraphicSegment],
        text_segments: [TextSegment],
        data_extension_segments: [DataExtensionSegment],
        reserved_extension_segments: [ReservedExtensionSegment]
    }
}