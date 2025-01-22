import os
import subprocess
from typing import List

def compile_file(command: List[str], current_dir: str):
    """
    Compile a file using a command provided as an argument.
    param directory: The directory where the input file is located
    param input_file: The name of the input file
    param output_folder_name: The name of the output folder
    param command: The command to run the compilation process ()
    """
    try:

        # Run the compilation process
        result = subprocess.run(
            command, 
            capture_output=True, 
            text=True, 
            check=True
        )
        
        # Successful compilation
        return {
            'success': True,
            'message': 'Compilation successful',
            'output_files': os.listdir(current_dir)
        }

    
    except Exception as e:
        # Catch any other unexpected errors
        return {
            'success': False,
            'message': f"Unexpected error during compilation: {str(e)}"
        }

def get_current_dir(directory, input_file, output_folder_name):

    # Ensure the directory exists
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory.")
        return

    # Create a directory for compiled C++ files
    cpp_output_dir = os.path.join(directory, output_folder_name)
    os.makedirs(cpp_output_dir, exist_ok=True)

    current_dir = os.path.join(cpp_output_dir, input_file.split('.')[0])
    os.makedirs(current_dir, exist_ok=True)
    return current_dir