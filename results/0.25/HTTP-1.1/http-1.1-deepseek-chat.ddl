-- HTTP/1.1 Protocol Specification in Daedalus

-- Define the HTTP version
let http_version = "HTTP/1.1"

-- Define the HTTP request method
let http_method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "PATCH"

-- Define the HTTP request URI
let http_uri = Many1 (NotChar " ")

-- Define the HTTP request line
let http_request_line = http_method <|> " " <|> http_uri <|> " " <|> http_version

-- Define the HTTP header field name
let http_header_name = Many1 (NotChar ":")

-- Define the HTTP header field value
let http_header_value = Many1 (NotChar "\r\n")

-- Define the HTTP header
let http_header = http_header_name <|> ": " <|> http_header_value

-- Define the HTTP headers
let http_headers = Many (http_header <|> "\r\n")

-- Define the HTTP request body
let http_body = Many1 AnyChar

-- Define the HTTP request
let http_request = http_request_line <|> "\r\n" <|> http_headers <|> "\r\n" <|> http_body

-- Define the HTTP status code
let http_status_code = Digit <|> Digit <|> Digit

-- Define the HTTP status message
let http_status_message = Many1 (NotChar "\r\n")

-- Define the HTTP response line
let http_response_line = http_version <|> " " <|> http_status_code <|> " " <|> http_status_message

-- Define the HTTP response
let http_response = http_response_line <|> "\r\n" <|> http_headers <|> "\r\n" <|> http_body

-- Define the HTTP message (either request or response)
let http_message = http_request | http_response

-- Entry point for parsing HTTP messages
let main = http_message