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

    def get_compile_data(self, table_name: str, llm: str, ddl: str, form: str):
        cursor = self.conn.cursor()
        cursor.execute(f"SELECT format, try FROM {table_name} WHERE compiled = 'True' AND llm = ? AND ddl = ? AND format = ? ORDER BY timestamp DESC LIMIT 1", (llm, ddl, form))
        return cursor.fetchall()
    
    def get_number_of_compiled(self, table_name: str, llm: str):
        cursor = self.conn.cursor()
        cursor.execute(f"SELECT format, ddl FROM {table_name} WHERE compiled = 'True' AND llm = ?", (llm,))
        return cursor.fetchall()
    
    def delete_tables(self):
        cursor = self.conn.cursor()
        cursor.execute(f"select name from sqlite_master where type='table'")
        return cursor.fetchall()
    
    def delete_table_with_name(self, table_name):
        cursor = self.conn.cursor()
        cursor.execute(f"drop table {table_name}")
        return cursor.fetchall()
    
    def get_lines_of_code(self, llms, ddl, form):
        temperatures = ["0_0", "0_25", "0_5", "0_75", "1_0"]
        return_dict = {}
        for llm in llms:
            return_values = []
            for temp in temperatures:
                table_name = f"t_{str(temp).replace('.', '_')}_999999"
                cursor = self.conn.cursor()
                cursor.execute(f"SELECT output_response FROM {table_name} WHERE compiled = 'True' AND llm = ? AND ddl = ? AND format = ? ORDER BY timestamp DESC LIMIT 1", (llm, ddl, form))
                values = cursor.fetchall()
                if len(values) > 0:
                    return_values.append(len(values[0][0].split('\n')))
                else:
                    return_values.append(0)
            return_dict[llm] = return_values
        return return_dict