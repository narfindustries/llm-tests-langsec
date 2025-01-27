module NITF {

  type U16 = uint(16)
  type U32 = uint(32)
  type ASCII = bytes

  type Header = struct {
    fileType : ASCII (len = 9)
    headerLength : U16
    fileLength : U32
    // Additional fields as per NITF specification
  }

  type ImageSegment = struct {
    imageID : ASCII (len = 10)
    imageDateAndTime : ASCII (len = 14)
    targetID : ASCII (len = 17)
    // Additional fields as per NITF specification
  }

  type DataExtensionSegment = struct {
    desID : ASCII (len = 25)
    desVersion : ASCII (len = 2)
    desLength : U32
    // Additional fields as per NITF specification
  }

  type TextSegment = struct {
    textID : ASCII (len = 7)
    textDateAndTime : ASCII (len = 14)
    textTitle : ASCII (len = 80)
    // Additional fields as per NITF specification
  }

  type NITFFile = struct {
    header : Header
    imageSegments : [ImageSegment] (len = header.numImageSegments)
    dataExtensionSegments : [DataExtensionSegment] (len = header.numDataExtensionSegments)
    textSegments : [TextSegment] (len = header.numTextSegments)
  }

  let parse_nitf = parser {
    let header = parse Header
    let imageSegments = array (header.numImageSegments) of parse ImageSegment
    let dataExtensionSegments = array (header.numDataExtensionSegments) of parse DataExtensionSegment
    let textSegments = array (header.numTextSegments) of parse TextSegment

    return NITFFile {
      header = header,
      imageSegments = imageSegments,
      dataExtensionSegments = dataExtensionSegments,
      textSegments = textSegments
    }
  }
}