import os
import subprocess
import json
import sys

def compile_dfdl_file(directory, dfdl_file):
    """
    Find and compile dfdl (.dfdl) files in the given directory to C++ STL code.
    
    :param directory: Path to the directory containing .dfdl files
    """
    # Ensure the directory exists
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory.")
        return

    # Create a directory for compiled C++ files
    cpp_output_dir = os.path.join(directory, 'dfdl_output')
    os.makedirs(cpp_output_dir, exist_ok=True)

    # Compile each .dfdl file
    full_dfdl_path = os.path.join(directory, dfdl_file)

    current_dir = os.path.join(cpp_output_dir, dfdl_file.split('.')[0])
    os.makedirs(current_dir, exist_ok=True)

    # Do we need to compile if the compiled file already exists?
    if any(file.endswith('.py') for file in os.listdir(current_dir)):
        print(f"Directory {current_dir} contains a .py file. Skipping compilation.")
        return

    try:
        print(full_dfdl_path)
        options = json.loads(open('options.json').read())
        # Construct the kaitai-struct-compiler command
        cmd = [
            options["compiler_paths"]["DFDL"],     # Assumes kaitai-struct-compiler is in PATH
            'generate', 'c', '-s', full_dfdl_path, current_dir
        ]
        

        # Run the compilation process
        result = subprocess.run(
            cmd, 
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

    except subprocess.CalledProcessError as e:
        # Compilation failed
        return {
            'success': False,
            'message': f"Compilation error:\nSTDERR: {e.stderr}",
            'error_code': e.returncode
        }
    
    except FileNotFoundError:
        # dfdl compiler not installed
        return {
            'success': False,
            'message': "dfdl Compiler not found. Please install it and ensure it's in your system PATH."
        }
    
    except Exception as e:
        # Catch any other unexpected errors
        return {
            'success': False,
            'message': f"Unexpected error during compilation: {str(e)}"
        }

def main():
    # Check if directory is provided as an argument
    if len(sys.argv) < 2:
        print("Usage: python dfdl_compiler.py <directory_path>")
        sys.exit(1)
    
    # Get directory path from command-line argument
    directory = sys.argv[1]
    
    # Run the compilation
    compile_dfdl_file(directory)

if __name__ == '__main__':
    main()