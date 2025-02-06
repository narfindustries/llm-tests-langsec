data GIF = GIF {
  signature :: [UInt8]{6},
  logicalScreenDescriptor :: LogicalScreenDescriptor,
  globalColorTable :: Maybe (Array UInt8),
  extensions :: [ExtensionBlock],
  images :: [ImageBlock],
  trailer :: UInt8
}

data LogicalScreenDescriptor = LogicalScreenDescriptor {
  width :: UInt16,
  height :: UInt16,
  packedFields :: PackedFields
}

data PackedFields = PackedFields {
  globalColorTableFlag :: Bool,
  colorResolution :: UInt8,
  sortFlag :: Bool,
  sizeOfGlobalColorTable :: UInt8
}

data ImageBlock = ImageBlock {
  imageDescriptor :: ImageDescriptor,
  localColorTable :: Maybe (Array UInt8),
  imageData :: [UInt8]
}

data ImageDescriptor = ImageDescriptor {
  imageSeparator :: UInt8,
  leftPosition :: UInt16,
  topPosition :: UInt16,
  width :: UInt16,
  height :: UInt16,
  packedFields :: ImagePackedFields
}

data ImagePackedFields = ImagePackedFields {
  localColorTableFlag :: Bool,
  interlaceFlag :: Bool,
  sortFlag :: Bool,
  sizeOfLocalColorTable :: UInt8
}

data ExtensionBlock = GraphicControlExtension | CommentExtension | PlainTextExtension | ApplicationExtension

data GraphicControlExtension = GraphicControlExtension {
  blockIdentifier :: UInt8,
  blockSize :: UInt8,
  packedFields :: GraphicControlPackedFields,
  delayTime :: UInt16,
  transparentColorIndex :: UInt8,
  terminator :: UInt8
}

data GraphicControlPackedFields = GraphicControlPackedFields {
  reserved :: UInt8,
  disposalMethod :: UInt8,
  userInputFlag :: Bool,
  transparentColorFlag :: Bool
}

data CommentExtension = CommentExtension {
  blockIdentifier :: UInt8,
  commentData :: [UInt8],
  terminator :: UInt8
}

data PlainTextExtension = PlainTextExtension {
  blockIdentifier :: UInt8,
  plainTextData :: [UInt8],
  terminator :: UInt8
}

data ApplicationExtension = ApplicationExtension {
  blockIdentifier :: UInt8,
  applicationIdentifier :: [UInt8]{8},
  applicationAuthenticationCode :: [UInt8]{3},
  applicationData :: [UInt8],
  terminator :: UInt8
}
