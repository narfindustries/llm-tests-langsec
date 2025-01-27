#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_parser(void) {
    // Basic tokens
    HParser *whitespace = h_whitespace(h_ch(' '));
    HParser *newline = h_ch('\n');
    HParser *identifier = h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch('_'), NULL));
    
    // Keywords
    HParser *create = h_token("CREATE", strlen("CREATE"));
    HParser *table = h_token("TABLE", strlen("TABLE"));
    HParser *insert = h_token("INSERT", strlen("INSERT"));
    HParser *into = h_token("INTO", strlen("INTO"));
    HParser *values = h_token("VALUES", strlen("VALUES"));
    HParser *select = h_token("SELECT", strlen("SELECT"));
    HParser *from = h_token("FROM", strlen("FROM"));
    HParser *where = h_token("WHERE", strlen("WHERE"));
    
    // Symbols
    HParser *lparen = h_ch('(');
    HParser *rparen = h_ch(')');
    HParser *comma = h_ch(',');
    HParser *semicolon = h_ch(';');
    HParser *equals = h_ch('=');
    HParser *star = h_ch('*');
    
    // Data types
    HParser *int_type = h_token("INT", strlen("INT"));
    HParser *text_type = h_token("TEXT", strlen("TEXT"));
    HParser *datatype = h_choice(int_type, text_type, NULL);
    
    // Column definition
    HParser *column_def = h_sequence(identifier, whitespace, datatype, NULL);
    HParser *column_list = h_sepBy1(column_def, h_sequence(comma, whitespace, NULL));
    
    // Value
    HParser *number = h_many1(h_ch_range('0', '9'));
    HParser *quoted_string = h_sequence(h_ch('\''), 
                                      h_many1(h_choice(h_ch_range('a', 'z'), 
                                                     h_ch_range('A', 'Z'),
                                                     h_ch_range('0', '9'),
                                                     h_ch('_'),
                                                     h_ch(' '),
                                                     NULL)),
                                      h_ch('\''),
                                      NULL);
    HParser *value = h_choice(number, quoted_string, NULL);
    HParser *value_list = h_sepBy1(value, h_sequence(comma, whitespace, NULL));
    
    // CREATE TABLE statement
    HParser *create_stmt = h_sequence(create, whitespace,
                                    table, whitespace,
                                    identifier, whitespace,
                                    lparen, whitespace,
                                    column_list, whitespace,
                                    rparen, semicolon,
                                    NULL);
    
    // INSERT statement
    HParser *insert_stmt = h_sequence(insert, whitespace,
                                    into, whitespace,
                                    identifier, whitespace,
                                    lparen, whitespace,
                                    value_list, whitespace,
                                    rparen, semicolon,
                                    NULL);
    
    // WHERE clause
    HParser *where_clause = h_sequence(where, whitespace,
                                     identifier, whitespace,
                                     equals, whitespace,
                                     value,
                                     NULL);
    
    // SELECT statement
    HParser *select_stmt = h_sequence(select, whitespace,
                                    h_choice(star, identifier, NULL), whitespace,
                                    from, whitespace,
                                    identifier,
                                    h_optional(h_sequence(whitespace, where_clause, NULL)),
                                    semicolon,
                                    NULL);
    
    // Complete SQL statement
    return h_choice(create_stmt, insert_stmt, select_stmt, NULL);
}

HParser* sql_parser;

void init_sql_parser(void) {
    sql_parser = init_parser();
}