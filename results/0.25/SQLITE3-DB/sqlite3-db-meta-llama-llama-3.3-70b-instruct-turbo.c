#include <hammer/hammer.h>
#include <sqlite3.h>

int main() {
    HAMMERCategoria categoria;
    categoria.nombre = "llama-llama-3";
    categoria.version = 3.3;
    categoria.id = "llama-llama-3.3-70b-instruct-turbo";
    categoria.descripcion = "Llama-llama-3.3-70b-instruct-turbo";
    categoria.tipo = HAMMERTipoCategoria::BASE_DE_DATOS;

    HAMMERDB db;
    db.conexion = sqlite3_open("hammer.db", &categoria);
    if (!db.conexion) {
        sqlite3_close(db.conexion);
        return 1;
    }

    HAMMERTabla tabla;
    tabla.nombre = "sqlite3-db-meta-llama-llama-3.3-70b-instruct-turbo";
    tabla.descripcion = "Tabla para almacenar metadatos de la base de datos SQLite3";
    tabla.tipo = HAMMERTipoTabla::METADATOS;

    sqlite3_stmt *stmt;
    const char *sql = "CREATE TABLE IF NOT EXISTS sqlite3-db-meta-llama-llama-3.3-70b-instruct-turbo (id INTEGER PRIMARY KEY, nombre TEXT, tipo TEXT)";
    int rc = sqlite3_prepare_v2(db.conexion, sql, -1, &stmt, 0);
    if (rc != SQLITE_OK) {
        sqlite3_close(db.conexion);
        return 1;
    }

    rc = sqlite3_step(stmt);
    if (rc != SQLITE_DONE) {
        sqlite3_finalize(stmt);
        sqlite3_close(db.conexion);
        return 1;
    }

    sqlite3_finalize(stmt);
    sqlite3_close(db.conexion);
    return 0;
}