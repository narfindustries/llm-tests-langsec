type http_1_1 = {
  include "internet_date_time";

  type Request_Line = {
    method: choice {
      get: "GET";
      head: "HEAD";
      post: "POST";
      put: "PUT";
      delete: "DELETE";
      connect: "CONNECT";
      options: "OPTIONS";
      trace: "TRACE";
    };
    request_uri: choice {
      absolute_uri: uri;
      relative_uri: string;
      asterisk: "*";
    };
    http_version: "HTTP/1.1";
  };

  type Request = {
    request_line: Request_Line;
    headers: repeated {
      header: choice {
        accept: "Accept: " & Accept_Header;
        accept_charset: "Accept-Charset: " & Accept_Charset_Header;
        accept_encoding: "Accept-Encoding: " & Accept_Encoding_Header;
        accept_language: "Accept-Language: " & Accept_Language_Header;
        authorization: "Authorization: " & Authorization_Header;
        expect: "Expect: " & Expect_Header;
        from: "From: " & From_Header;
        host: "Host: " & Host_Header;
        if_match: "If-Match: " & If_Match_Header;
        if_modified_since: "If-Modified-Since: " & If_Modified_Since_Header;
        if_none_match: "If-None-Match: " & If_None_Match_Header;
        if_range: "If-Range: " & If_Range_Header;
        if_unmodified_since: "If-Unmodified-Since: " & If_Unmodified_Since_Header;
        max_forwards: "Max-Forwards: " & Max_Forwards_Header;
        proxy_authorization: "Proxy-Authorization: " & Proxy_Authorization_Header;
        range: "Range: " & Range_Header;
        referer: "Referer: " & Referer_Header;
        te: "TE: " & TE_Header;
        user_agent: "User-Agent: " & User_Agent_Header;
      };
    };
    body: bytes;
  };

  type Response = {
    status_line: Status_Line;
    headers: repeated {
      header: choice {
        accept_ranges: "Accept-Ranges: " & Accept_Ranges_Header;
        age: "Age: " & Age_Header;
        etag: "ETag: " & ETag_Header;
        location: "Location: " & Location_Header;
        proxy_authenticate: "Proxy-Authenticate: " & Proxy_Authenticate_Header;
        retry_after: "Retry-After: " & Retry_After_Header;
        server: "Server: " & Server_Header;
        vary: "Vary: " & Vary_Header;
        www_authenticate: "WWW-Authenticate: " & WWW_Authenticate_Header;
      };
    };
    body: bytes;
  };

  type Status_Line = {
    http_version: "HTTP/1.1";
    status_code: choice {
      informational: choice {
        continue: "100 Continue";
        switching_protocols: "101 Switching Protocols";
      };
      successful: choice {
        ok: "200 OK";
        created: "201 Created";
        accepted: "202 Accepted";
        non_authoritative_information: "203 Non-Authoritative Information";
        no_content: "204 No Content";
        reset_content: "205 Reset Content";
        partial_content: "206 Partial Content";
      };
      redirection: choice {
        multiple_choices: "300 Multiple Choices";
        moved_permanently: "301 Moved Permanently";
        found: "302 Found";
        see_other: "303 See Other";
        not_modified: "304 Not Modified";
        use_proxy: "305 Use Proxy";
        temporary_redirect: "307 Temporary Redirect";
      };
      client_error: choice {
        bad_request: "400 Bad Request";
        unauthorized: "401 Unauthorized";
        payment_required: "402 Payment Required";
        forbidden: "403 Forbidden";
        not_found: "404 Not Found";
        method_not_allowed: "405 Method Not Allowed";
        not_acceptable: "406 Not Acceptable";
        proxy_authentication_required: "407 Proxy Authentication Required";
        request_time_out: "408 Request Time-out";
        conflict: "409 Conflict";
        gone: "410 Gone";
        length_required: "411 Length Required";
        precondition_failed: "412 Precondition Failed";
        request_entity_too_large: "413 Request Entity Too Large";
        request_uri_too_large: "414 Request-URI Too Large";
        unsupported_media_type: "415 Unsupported Media Type";
        requested_range_not_satisfiable: "416 Requested range not satisfiable";
        expectation_failed: "417 Expectation Failed";
      };
      server_error: choice {
        internal_server_error: "500 Internal Server Error";
        not_implemented: "501 Not Implemented";
        bad_gateway: "502 Bad Gateway";
        service_unavailable: "503 Service Unavailable";
        gateway_time_out: "504 Gateway Time-out";
        http_version_not_supported: "505 HTTP Version not supported";
      };
    };
    reason_phrase: string;
  };

  type Accept_Header = {
    media_range: repeated {
      media_type: choice {
        type: string;
        subtype: string;
      };
      parameters: repeated {
        parameter: choice {
          q: "q=" & float;
          type: string;
          value: string;
        };
      };
    };
  };

  type Accept_Charset_Header = {
    charset: repeated {
      value: choice {
        charset_name: string;
        q: "q=" & float;
      };
    };
  };

  type Accept_Encoding_Header = {
    encoding: repeated {
      value: choice {
        encoding_name: string;
        q: "q=" & float;
      };
    };
  };

  type Accept_Language_Header = {
    language_range: repeated {
      value: choice {
        language_tag: string;
        q: "q=" & float;
      };
    };
  };

  type Authorization_Header = {
    credentials: choice {
      basic: "Basic " & string;
      digest: "Digest " & string;
    };
  };

  type Expect_Header = {
    expectation: choice {
      continue: "100-continue";
    };
  };

  type From_Header = {
    mailbox: string;
  };

  type Host_Header = {
    host: choice {
      domain_name: string;
      ip_address: string;
      port: integer;
    };
  };

  type If_Match_Header = {
    entity_tags: repeated {
      tag: string;
    };
  };

  type If_Modified_Since_Header = {
    date: internet_date_time;
  };

  type If_None_Match_Header = {
    entity_tags: repeated {
      tag: string;
    };
  };

  type If_Range_Header = {
    entity_tags: repeated {
      tag: string;
    };
    date: internet_date_time;
  };

  type If_Unmodified_Since_Header = {
    date: internet_date_time;
  };

  type Max_Forwards_Header = {
    max_forwards: integer;
  };

  type Proxy_Authorization_Header = {
    credentials: choice {
      basic: "Basic " & string;
      digest: "Digest " & string;
    };
  };

  type Range_Header = {
    ranges: repeated {
      range: choice {
        bytes: "bytes=" & integer & "-" & integer;
      };
    };
  };

  type Referer_Header = {
    referer: string;
  };

  type TE_Header = {
    transfer_encoding: repeated {
      encoding: choice {
        encoding_name: string;
        q: "q=" & float;
      };
    };
  };

  type User_Agent_Header = {
    product_tokens: repeated {
      token: string;
    };
  };

  type Accept_Ranges_Header = {
    accept_ranges: choice {
      bytes: "bytes";
      none: "none";
    };
  };

  type Age_Header = {
    age: integer;
  };

  type ETag_Header = {
    entity_tag: string;
  };

  type Location_Header = {
    location: string;
  };

  type Proxy_Authenticate_Header = {
    challenge: choice {
      basic: "Basic realm=\"" & string & "\"";
      digest: "Digest realm=\"" & string & "\"";
    };
  };

  type Retry_After_Header = {
    retry_after: choice {
      date: internet_date_time;
      delay: integer;
    };
  };

  type Server_Header = {
    product_tokens: repeated {
      token: string;
    };
  };

  type Vary_Header = {
    field_names: repeated {
      field_name: string;
    };
  };

  type WWW_Authenticate_Header = {
    challenge: choice {
      basic: "Basic realm=\"" & string & "\"";
      digest: "Digest realm=\"" & string & "\"";
    };
  };
}