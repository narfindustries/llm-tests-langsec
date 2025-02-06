meta:
  id: http_1_1
  title: HTTP 1.1

seq:
  - id: request_line
    type: request_line
  - id: headers
    type: headers
  - id: body
    type: body
    if: content_length > 0

types:
  request_line:
    seq:
      - id: method
        type: str
        encoding: ascii
        terminator: 32
      - id: request_uri
        type: str
        encoding: ascii
        terminator: 32
      - id: http_version
        type: str
        encoding: ascii
        terminator: 10

  headers:
    seq:
      - id: header
        type: header
        repeat: until [10, 10]

  header:
    seq:
      - id: name
        type: str
        encoding: ascii
        terminator: 58
      - id: value
        type: str
        encoding: ascii
        terminator: 10

  body:
    seq:
      - id: content
        type: bytes
        size: content_length

  response_line:
    seq:
      - id: http_version
        type: str
        encoding: ascii
        terminator: 32
      - id: status_code
        type: uint
        size: 32
      - id: reason_phrase
        type: str
        encoding: ascii
        terminator: 10

  response:
    seq:
      - id: response_line
        type: response_line
      - id: headers
        type: headers
      - id: body
        type: body
        if: content_length > 0

instances:
  content_length:
    value: content_length_header.content_length

  content_length_header:
    seq:
      - id: content_length
        type: uint
        size: 32

  host_header:
    seq:
      - id: host
        type: str
        encoding: ascii
        terminator: 10

  accept_header:
    seq:
      - id: accept
        type: str
        encoding: ascii
        terminator: 10

  accept_charset_header:
    seq:
      - id: accept_charset
        type: str
        encoding: ascii
        terminator: 10

  accept_encoding_header:
    seq:
      - id: accept_encoding
        type: str
        encoding: ascii
        terminator: 10

  accept_language_header:
    seq:
      - id: accept_language
        type: str
        encoding: ascii
        terminator: 10

  authorization_header:
    seq:
      - id: authorization
        type: str
        encoding: ascii
        terminator: 10

  cache_control_header:
    seq:
      - id: cache_control
        type: str
        encoding: ascii
        terminator: 10

  connection_header:
    seq:
      - id: connection
        type: str
        encoding: ascii
        terminator: 10

  content_encoding_header:
    seq:
      - id: content_encoding
        type: str
        encoding: ascii
        terminator: 10

  content_language_header:
    seq:
      - id: content_language
        type: str
        encoding: ascii
        terminator: 10

  content_type_header:
    seq:
      - id: content_type
        type: str
        encoding: ascii
        terminator: 10

  date_header:
    seq:
      - id: date
        type: str
        encoding: ascii
        terminator: 10

  expect_header:
    seq:
      - id: expect
        type: str
        encoding: ascii
        terminator: 10

  from_header:
    seq:
      - id: from
        type: str
        encoding: ascii
        terminator: 10

  if_match_header:
    seq:
      - id: if_match
        type: str
        encoding: ascii
        terminator: 10

  if_modified_since_header:
    seq:
      - id: if_modified_since
        type: str
        encoding: ascii
        terminator: 10

  if_none_match_header:
    seq:
      - id: if_none_match
        type: str
        encoding: ascii
        terminator: 10

  if_range_header:
    seq:
      - id: if_range
        type: str
        encoding: ascii
        terminator: 10

  if_unmodified_since_header:
    seq:
      - id: if_unmodified_since
        type: str
        encoding: ascii
        terminator: 10

  max_forwards_header:
    seq:
      - id: max_forwards
        type: uint
        size: 32

  proxy_authorization_header:
    seq:
      - id: proxy_authorization
        type: str
        encoding: ascii
        terminator: 10

  range_header:
    seq:
      - id: range
        type: str
        encoding: ascii
        terminator: 10

  referer_header:
    seq:
      - id: referer
        type: str
        encoding: ascii
        terminator: 10

  te_header:
    seq:
      - id: te
        type: str
        encoding: ascii
        terminator: 10

  upgrade_header:
    seq:
      - id: upgrade
        type: str
        encoding: ascii
        terminator: 10

  user_agent_header:
    seq:
      - id: user_agent
        type: str
        encoding: ascii
        terminator: 10

  via_header:
    seq:
      - id: via
        type: str
        encoding: ascii
        terminator: 10

  warning_header:
    seq:
      - id: warning
        type: str
        encoding: ascii
        terminator: 10

  status_code_100:
    value: 100

  status_code_101:
    value: 101

  status_code_200:
    value: 200

  status_code_201:
    value: 201

  status_code_202:
    value: 202

  status_code_203:
    value: 203

  status_code_204:
    value: 204

  status_code_205:
    value: 205

  status_code_206:
    value: 206

  status_code_300:
    value: 300

  status_code_301:
    value: 301

  status_code_302:
    value: 302

  status_code_303:
    value: 303

  status_code_304:
    value: 304

  status_code_305:
    value: 305

  status_code_307:
    value: 307

  status_code_400:
    value: 400

  status_code_401:
    value: 401

  status_code_402:
    value: 402

  status_code_403:
    value: 403

  status_code_404:
    value: 404

  status_code_405:
    value: 405

  status_code_406:
    value: 406

  status_code_407:
    value: 407

  status_code_408:
    value: 408

  status_code_409:
    value: 409

  status_code_410:
    value: 410

  status_code_411:
    value: 411

  status_code_412:
    value: 412

  status_code_413:
    value: 413

  status_code_414:
    value: 414

  status_code_415:
    value: 415

  status_code_416:
    value: 416

  status_code_417:
    value: 417

  status_code_500:
    value: 500

  status_code_501:
    value: 501

  status_code_502:
    value: 502

  status_code_503:
    value: 503

  status_code_504:
    value: 504

  status_code_505:
    value: 505