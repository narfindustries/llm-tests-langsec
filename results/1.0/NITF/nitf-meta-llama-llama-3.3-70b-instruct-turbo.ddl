domain nitf {
  namespace "http://example.com/nitf"
  version "3.3"

  type MetaData {
    id: string @key
    title: string
    description: string
    creator: string
    created: date
  }

  type Image {
    id: string @key
    meta: MetaData
    data: bytes
  }

  type NITF {
    header: Header
    images: Image*
    meta: MetaData
  }

  type Header {
    file_id: string
    file_version: string
    file_date: date
  }
}

mapping NITF {
  // header
  header.file_id = string(9) @position(0)
  header.file_version = string(2) @position(9)
  header.file_date = date @position(11)

  // meta
  meta.id = string(25) @position(37)
  meta.title = string(65) @position(62)
  meta.description = string(255) @position(127)
  meta.creator = string(32) @position(382)
  meta.created = date @position(414)

  // images
  images -> array(@position(446)) {
    id = string(10) @position(0)
    meta -> MetaData @position(10)
    data = bytes(@position(435))
  }
}