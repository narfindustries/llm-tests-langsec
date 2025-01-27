#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Database configuration structure
typedef struct {
    sqlite3 *db;
    char *dbPath;
} DatabaseConfig;

// Parser for database configuration
static HParser *database_config_parser() {
    return h_sequence(
        h_token_ci("DATABASE"),
        h_whitespace(),
        h_quoted_string(),
        NULL
    );
}

// Database initialization function
static int initialize_database(DatabaseConfig *config) {
    int rc = sqlite3_open(config->dbPath, &config->db);
    if (rc != SQLITE_OK) {
        fprintf(stderr, "Cannot open database: %s\n", sqlite3_errmsg(config->db));
        return -1;
    }
    return 0;
}

// Query execution function
static int execute_query(DatabaseConfig *config, const char *query) {
    char *errMsg = 0;
    int rc = sqlite3_exec(config->db, query, 0, 0, &errMsg);
    
    if (rc != SQLITE_OK) {
        fprintf(stderr, "SQL error: %s\n", errMsg);
        sqlite3_free(errMsg);
        return -1;
    }
    return 0;
}

// Table creation function
static int create_tables(DatabaseConfig *config) {
    const char *create_users_table = 
        "CREATE TABLE IF NOT EXISTS users ("
        "id INTEGER PRIMARY KEY AUTOINCREMENT, "
        "username TEXT NOT NULL UNIQUE, "
        "email TEXT NOT NULL UNIQUE, "
        "created_at DATETIME DEFAULT CURRENT_TIMESTAMP)";
    
    const char *create_logs_table = 
        "CREATE TABLE IF NOT EXISTS logs ("
        "id INTEGER PRIMARY KEY AUTOINCREMENT, "
        "user_id INTEGER, "
        "action TEXT, "
        "timestamp DATETIME DEFAULT CURRENT_TIMESTAMP, "
        "FOREIGN KEY(user_id) REFERENCES users(id))";
    
    if (execute_query(config, create_users_table) != 0) {
        return -1;
    }
    
    if (execute_query(config, create_logs_table) != 0) {
        return -1;
    }
    
    return 0;
}

// User insertion function
static int insert_user(DatabaseConfig *config, const char *username, const char *email) {
    sqlite3_stmt *stmt;
    const char *query = "INSERT INTO users (username, email) VALUES (?, ?)";
    
    if (sqlite3_prepare_v2(config->db, query, -1, &stmt, 0) != SQLITE_OK) {
        fprintf(stderr, "Failed to prepare statement\n");
        return -1;
    }
    
    sqlite3_bind_text(stmt, 1, username, -1, SQLITE_STATIC);
    sqlite3_bind_text(stmt, 2, email, -1, SQLITE_STATIC);
    
    if (sqlite3_step(stmt) != SQLITE_DONE) {
        fprintf(stderr, "Failed to insert user\n");
        sqlite3_finalize(stmt);
        return -1;
    }
    
    sqlite3_finalize(stmt);
    return 0;
}

// Log insertion function
static int insert_log(DatabaseConfig *config, int user_id, const char *action) {
    sqlite3_stmt *stmt;
    const char *query = "INSERT INTO logs (user_id, action) VALUES (?, ?)";
    
    if (sqlite3_prepare_v2(config->db, query, -1, &stmt, 0) != SQLITE_OK) {
        fprintf(stderr, "Failed to prepare statement\n");
        return -1;
    }
    
    sqlite3_bind_int(stmt, 1, user_id);
    sqlite3_bind_text(stmt, 2, action, -1, SQLITE_STATIC);
    
    if (sqlite3_step(stmt) != SQLITE_DONE) {
        fprintf(stderr, "Failed to insert log\n");
        sqlite3_finalize(stmt);
        return -1;
    }
    
    sqlite3_finalize(stmt);
    return 0;
}

// Database cleanup function
static void cleanup_database(DatabaseConfig *config) {
    if (config->db) {
        sqlite3_close(config->db);
    }
    free(config->dbPath);
}

// Main application logic
int main() {
    DatabaseConfig config = {0};
    config.dbPath = strdup("example.db");
    
    if (initialize_database(&config) != 0) {
        cleanup_database(&config);
        return -1;
    }
    
    if (create_tables(&config) != 0) {
        cleanup_database(&config);
        return -1;
    }
    
    // Example user and log insertions
    if (insert_user(&config, "johndoe", "john@example.com") != 0) {
        cleanup_database(&config);
        return -1;
    }
    
    int user_id = sqlite3_last_insert_rowid(config.db);
    
    if (insert_log(&config, user_id, "user_registration") != 0) {
        cleanup_database(&config);
        return -1;
    }
    
    cleanup_database(&config);
    return 0;
}