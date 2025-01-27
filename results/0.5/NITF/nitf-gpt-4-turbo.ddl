module NITF {

  type U16 = uint(16)
  type U32 = uint(32)
  type ASCII = bytes

  type Header = struct {
    fileType : ASCII[9]
    fileVersion : ASCII[2]
    complexFlag : ASCII[1]
    systemType : ASCII[4]
    originStationID : ASCII[10]
    fileDateTime : ASCII[14]
    fileTitle : ASCII[80]
    fileSecurity : ASCII[1]
    copyNumber : ASCII[5]
    origNumber : ASCII[5]
    encryption : ASCII[1]
    fileLength : ASCII[12]
    headerLength : ASCII[6]
  }

  type ImageSegment = struct {
    imageID : ASCII[10]
    dateTime : ASCII[14]
    targetID : ASCII[17]
    title : ASCII[80]
    securityClass : ASCII[1]
    encryption : ASCII[1]
    imageFormat : ASCII[3]
    numBlocksPerRow : U16
    numBlocksPerCol : U16
    numPixelsPerBlockHorz : U16
    numPixelsPerBlockVert : U16
    numBitsPerPixel : U16
    imageLength : U32
    imageWidth : U32
    data : bytes(imageLength)
  }

  type Data = struct {
    header : Header
    imageSegments : [ImageSegment] // Assuming multiple image segments can be present
  }

  let parse = Data.parseFile
}