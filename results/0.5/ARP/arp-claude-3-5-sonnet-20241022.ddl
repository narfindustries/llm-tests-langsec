def Main = {
  def line = (many1 {
    | ' ' => ' '
    | '$' => '$'
    | '!' => '!'
    | '?' => '?'
    | '.' => '.'
    | ',' => ','
    | ';' => ';'
    | ':' => ':'
    | [a-zA-Z] => $0
  }) ^^ `String`

  def emptyLine = empty ^^ ``

  def stanza = many1 {line} && emptyLine

  many1 {stanza}
}