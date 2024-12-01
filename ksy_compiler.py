import os
import subprocess
import sys

def compile_ksy_files(directory):
    """
    Find and compile Kaitai Struct (.ksy) files in the given directory to C++ STL code.
    
    :param directory: Path to the directory containing .ksy files
    """
    # Ensure the directory exists
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory.")
        return

    # Find all .ksy files in the directory
    ksy_files = [f for f in os.listdir(directory) if f.endswith('.ksy')]
    
    if not ksy_files:
        print(f"No .ksy files found in {directory}")
        return

    # Create a directory for compiled C++ files
    cpp_output_dir = os.path.join(directory, 'kaitai_output')
    os.makedirs(cpp_output_dir, exist_ok=True)

    # Compile each .ksy file
    for ksy_file in ksy_files:
        full_ksy_path = os.path.join(directory, ksy_file)
        
        try:
            current_dir = os.path.join(cpp_output_dir, ksy_file.split('.')[0])
            os.makedirs(current_dir, exist_ok=True)
            # Construct the kaitai-struct-compiler command
            cmd = [
                'kaitai-struct-compiler',  # Assumes kaitai-struct-compiler is in PATH
                '-t', 'cpp_stl',           # Target C++ STL
                '--outdir', current_dir,      # Destination directory
                full_ksy_path
            ]
            
            # Run the compilation command
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            # Check for successful compilation
            if result.returncode == 0:
                print(f"Successfully compiled {ksy_file}")
                # Optionally print compiler output
                if result.stdout:
                    print("Compiler output:", result.stdout)
            else:
                print(f"Error compiling {ksy_file}")
                print("Error output:", result.stderr)
        
        except FileNotFoundError:
            print("Error: kaitai-struct-compiler not found. Ensure it's installed and in your PATH.")
            return
        except Exception as e:
            print(f"Unexpected error compiling {ksy_file}: {e}")

def main():
    # Check if directory is provided as an argument
    if len(sys.argv) < 2:
        print("Usage: python ksy_compiler.py <directory_path>")
        sys.exit(1)
    
    # Get directory path from command-line argument
    directory = sys.argv[1]
    
    # Run the compilation
    compile_ksy_files(directory)

if __name__ == '__main__':
    main()