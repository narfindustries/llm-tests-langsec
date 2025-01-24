import sqlite3

class Database:
    
    def __init__(self, dbname: str):
        self.conn = sqlite3.connect(dbname)

    def create_table(self, table_name: str):
        columns = ['timestamp', 'llm', 'ddl', 'format', 'compiled', 'try', 'output_response']
        cursor = self.conn.cursor()
        cursor.execute(f"CREATE TABLE IF NOT EXISTS {table_name} ({', '.join(columns)})")
        columns_with_types = [
            'timestamp TEXT',
            'llm TEXT',
            'ddl TEXT',
            'format TEXT',
            'compiled TEXT',
            'try INTEGER',
            'output_response TEXT'
        ]
        cursor.execute(f"CREATE TABLE IF NOT EXISTS {table_name} ({', '.join(columns_with_types)})")
        self.conn.commit()

    def insert_data(self, table_name: str, data: dict):
        cursor = self.conn.cursor()
        cursor.execute(f"INSERT INTO {table_name} (timestamp, llm, ddl, format, compiled, try, output_response) VALUES (?, ?, ?, ?, ?, ?, ?)", tuple(data.values()))
        self.conn.commit()

    def get_compile_data(self, table_name: str):
        cursor = self.conn.cursor()
        cursor.execute(f"SELECT llm, ddl, compiled, try FROM {table_name} LIMIT 10")
        return cursor.fetchall()