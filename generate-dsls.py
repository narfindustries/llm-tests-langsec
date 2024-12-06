import os
import requests
import google.generativeai as genai
import anthropic
from openai import OpenAI
from typing import Dict, Any, List
import json
from ksy_compiler import compile_ksy_file

class LLMFormatGeneration:
    def __init__(self):
        # Initialize API clients
        self.openai_client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        self.anthropic_client = anthropic.Anthropic(api_key=os.getenv('ANTHROPIC_API_KEY'))
        self.deepseek_api_key = os.getenv('DEEPSEEK_API_KEY')
        genai.configure(api_key=os.getenv('GOOGLE_API_KEY'))
        self.llms = {
            "gemini-pro": self.call_gemini_api,
            "gpt-4-turbo": self.call_gpt_api,
            "claude-3-opus-20240229": self.call_claude_api,
            "deepseek": self.call_deepseek_api
        }

    def call_gpt_api(self, query: str, model: str) -> str:
        """Call OpenAI's GPT API"""
        try:
            response = self.openai_client.chat.completions.create(
                model=model,
                max_tokens=4096,
                messages=[
                    {"role": "system", "content": "You are a helpful assistant."},
                    {"role": "user", "content": query}
                ],
                temperature=0.4
            )
            return response.choices[0].message.content
        except Exception as e:
            return f"GPT API Error: {str(e)}"

    def call_claude_api(self, query: str, model: str) -> str:
        """Call Anthropic's Claude API"""
        try:
            response = self.anthropic_client.messages.create(
                model=model,
                max_tokens=4096,
                messages=[
                    {"role": "user", "content": query}
                ],
                temperature=0.4,
            )
            return response.content[0].text
        except Exception as e:
            return f"Claude API Error: {str(e)}"

    def call_deepseek_api(self, query: str, model: str) -> str:
        """Call Deepseek Coder API"""
        try:
            response = requests.post(
                "https://api.deepseek.com/v1/chat/completions",
                headers={
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {self.deepseek_api_key}"
                },
                json={
                    "model": "deepseek-coder",
                    "messages": [
                        {"role": "system", "content": "You are a software developer who has read standards for several network protocols and file formats and knows the syntax of Data Description Languages like Kaitai Struct, DaeDalus, DFDL, and Parsley"},
                        {"role": "user", "content": query}
                    ],
                    "temperature": 0.4,
                    "max_tokens": 4096
                }
            )
            return response.json()['choices'][0]['message']['content']
        except Exception as e:
            return f"Deepseek API Error: {str(e)}"

    def call_gemini_api(self, query: str, model: str) -> str:
        """Call Google Gemini API"""
        try:
            generation_config = {
                "temperature": 0.4,  # Most deterministic setting
                "max_output_tokens": 4096
            }
            model = genai.GenerativeModel(model)
            response = model.generate_content(query, generation_config=generation_config)
            return response.text
        except Exception as e:
            return f"Gemini API Error: {str(e)}"

def generate_specifications_per_format(format: str, specification: str, ddl: str, output: str, extension: str):
    comparison = LLMFormatGeneration()
    
    # Specify the two queries needed
    query_1 = f"You are a software developer who has read the {specification} for the {format}. Can you list all the fields in the specification along with all the values each field can take?"
    query_2 = f"Can you use this knowledge to generate a {ddl} specification for the {format} in {output} format? Make sure to cover the entire specification including any optional fields. Do not provide any text response other than the {format} specification. Show only the complete response. Do not wrap the response in any markdown."

    for model, function in comparison.llms.items():
        # The format names contain spaces, so change them all to hyphens    
        filename = f"{format.lower().replace(' ', '-')}-{model.lower().replace(' ', '-')}.{extension}"
        if not os.path.exists(f"generated/{format}/{filename}"):
            print(format, filename)
            response1 = function(query_1, model)
            print(response1) #, response2)
            response2 = function(query_2, model)
            create_response_file(response2, format, filename)
        compilation_output = compile_ksy_file(f"generated/{format}/", filename)
        counter = 1
        while not compilation_output["success"] and counter != 0:
            counter = counter - 1
            message = compilation_output['message'].replace("\n", " ")
            query_3 = f"The previous response gave me an error. Can you use this error message: \"{message}\" to improve the specification and give me an improved and fixed {ddl} specification in {output} format. Give me only the generated code."
            response3 = function(query_3, model)
            create_response_file(response3, format, filename)
            # Overwrite the compilation_output variable
            compilation_output = compile_ksy_file(f"generated/{format}/", filename)
    
def create_response_file(response: str, format: str, filename: str):
    """
    Check if there was an exception in the LLM command
    If not, then create a new file
    """
    if not "Error" in response: # Ensure that responses with errors do not get written
        file_desc = open(f"generated/{format}/{filename}", "w")
        # Some LLMs give you markdown encapsulated code
        # We are removing the lines that start with these characters
        lines = response.split("\n")
        cleaned_lines = [line for line in lines if not line.strip().startswith('```') and not line.strip().startswith('---')]
        response = "\n".join(cleaned_lines)
        file_desc.write(response)
        file_desc.close()
        # Write the output into the corresponding files

def main():

    # Parse the file with the options
    parsed_options = json.loads(open("options.json").read())
    # Combining parsing the file-formats and network protocols in one loop to make things easier
    for format, spec in (parsed_options["file-formats"].items() | parsed_options["network-protocols"].items()):
        dir_path = f"generated/{format}"
        if not os.path.exists(dir_path):
            os.makedirs(dir_path)

        # The last three arguments would actually come from the parsed_options["DDLs"] field
        generate_specifications_per_format(format, spec, "Kaitai struct", "yaml", "ksy")

if __name__ == "__main__":
    main()