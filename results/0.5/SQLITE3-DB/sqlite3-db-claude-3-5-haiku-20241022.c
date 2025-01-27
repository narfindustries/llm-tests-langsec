#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Database configuration
#define DB_PATH "claude_database.sqlite"
#define MAX_QUERY_LENGTH 1024

// Parser and database interaction structures
typedef struct {
    sqlite3 *db;
    char last_error[256];
} DatabaseContext;

// Hammer parsing definitions
static HParser *json_parser;
static HParser *record_parser;

// Error handling function
void handle_sqlite_error(DatabaseContext *ctx, int rc) {
    if (rc != SQLITE_OK) {
        snprintf(ctx->last_error, sizeof(ctx->last_error), 
                 "SQLite error: %s", sqlite3_errmsg(ctx->db));
    }
}

// Database initialization function
int initialize_database(DatabaseContext *ctx) {
    int rc = sqlite3_open(DB_PATH, &ctx->db);
    if (rc != SQLITE_OK) {
        handle_sqlite_error(ctx, rc);
        return 0;
    }

    const char *create_table_sql = 
        "CREATE TABLE IF NOT EXISTS records ("
        "id INTEGER PRIMARY KEY AUTOINCREMENT, "
        "name TEXT, "
        "value REAL, "
        "timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)";

    rc = sqlite3_exec(ctx->db, create_table_sql, 0, 0, 0);
    handle_sqlite_error(ctx, rc);
    
    return rc == SQLITE_OK;
}

// Insert record function
int insert_record(DatabaseContext *ctx, const char *name, double value) {
    sqlite3_stmt *stmt;
    const char *insert_sql = 
        "INSERT INTO records (name, value) VALUES (?, ?)";
    
    int rc = sqlite3_prepare_v2(ctx->db, insert_sql, -1, &stmt, 0);
    if (rc != SQLITE_OK) {
        handle_sqlite_error(ctx, rc);
        return 0;
    }

    sqlite3_bind_text(stmt, 1, name, -1, SQLITE_STATIC);
    sqlite3_bind_double(stmt, 2, value);

    rc = sqlite3_step(stmt);
    sqlite3_finalize(stmt);

    return rc == SQLITE_DONE;
}

// JSON parsing function using Hammer
HParsedToken* parse_json_record(DatabaseContext *ctx, const char *json_str) {
    const HParseResult *result = h_parse(json_parser, 
        (const uint8_t*)json_str, strlen(json_str));
    
    if (!result || !result->ast) {
        snprintf(ctx->last_error, sizeof(ctx->last_error), 
                 "JSON parsing failed");
        return NULL;
    }

    return result->ast;
}

// Main parsing and database interaction logic
int process_records(DatabaseContext *ctx, const char *json_input) {
    HParsedToken *parsed_record = parse_json_record(ctx, json_input);
    if (!parsed_record) {
        return 0;
    }

    // Assuming parsed record has name and value fields
    const char *name = parsed_record->str;
    double value = parsed_record->number;

    return insert_record(ctx, name, value);
}

// Cleanup function
void cleanup_database(DatabaseContext *ctx) {
    if (ctx->db) {
        sqlite3_close(ctx->db);
    }
}

// JSON parser setup
void setup_json_parser() {
    json_parser = h_choice(
        h_sequence(
            h_token("{", 1),
            h_token("}", 1),
            NULL
        ),
        h_sequence(
            h_token("{", 1),
            h_many(h_token("\"", 1)),
            h_token("}", 1),
            NULL
        ),
        NULL
    );
}

int main() {
    DatabaseContext ctx = {0};
    
    setup_json_parser();

    if (!initialize_database(&ctx)) {
        fprintf(stderr, "Database initialization failed: %s\n", ctx.last_error);
        return 1;
    }

    // Example JSON processing
    const char *sample_json = "{\"name\":\"example\",\"value\":42.5}";
    if (!process_records(&ctx, sample_json)) {
        fprintf(stderr, "Record processing failed: %s\n", ctx.last_error);
    }

    cleanup_database(&ctx);
    return 0;
}