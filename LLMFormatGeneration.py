import requests
import google.generativeai as genai
import anthropic
import os
from openai import OpenAI
from together import Together
import json

class LLMFormatGeneration:
    def __init__(self, temperature: float):
        self.system_prompt = "You are a software developer who has read standards for several network protocols and file formats and knows the syntax of Data Description Languages like Kaitai Struct, DaeDalus, DFDL, and Zeek Spicy."
        self.temperature = temperature
        # Initialize API clients
        self.openai_client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        self.anthropic_client = anthropic.Anthropic(api_key=os.getenv('ANTHROPIC_API_KEY'))
        self.deepseek_api_key = os.getenv('DEEPSEEK_API_KEY')
        genai.configure(api_key=os.getenv('GOOGLE_API_KEY'))
        self.together_api_key = os.getenv('TOGETHER_API_KEY')
        self.llms = {
            "gemini-1.5-flash": self.call_gemini_api,
            "gpt-4-turbo": self.call_gpt_api,
            "gpt-4o": self.call_gpt_api,
            "claude-3-5-sonnet-20241022": self.call_claude_api,
            "claude-3-5-haiku-20241022": self.call_claude_api,
            "deepseek-chat": self.call_deepseek_api,
            "meta-llama/Llama-3.3-70B-Instruct-Turbo": self.call_llama3_api
        }

    def call_gpt_api(self, query: str, model: str) -> str:
        """Call OpenAI's GPT API"""
        try:
            response = self.openai_client.chat.completions.create(
                model=model,
                max_tokens=4096,
                messages=[
                    {"role": "system", "content": self.system_prompt},
                    {"role": "user", "content": query}
                ],
                temperature=self.temperature
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
                temperature=self.temperature,
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
                        {"role": "system", "content": self.system_prompt},
                        {"role": "user", "content": query}
                    ],
                    "temperature": self.temperature,
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
                "temperature": self.temperature,
                "max_output_tokens": 4096
            }
            model = genai.GenerativeModel(model)
            response = model.generate_content(query, generation_config=generation_config)
            return response.text
        except Exception as e:
            return f"Gemini API Error: {str(e)}"
        

    def call_llama3_api(self, query: str, model: str) -> str:
        """Call Llama3 through together API"""
        try:
            payload = {
                    "model": model,
                    "response_format": { "type": "json_object" },
                    "temperature": self.temperature,
                    "frequency_penalty": 0,
                    "presence_penalty": 0,
                    "messages": [
                        {
                            "role": "system",
                            "content": self.system_prompt
                        },
                        {
                            "role": "user",
                            "content": query
                        }
                    ],
                    "stream": False,
                    "max_tokens": 4096
                }
            headers = {
                    "accept": "application/json",
                    "content-type": "application/json",
                    "Authorization": (f"Bearer {self.together_api_key}")
                }

            response = requests.post("https://api.together.xyz/v1/chat/completions", json=payload, headers=headers)
            api_response = json.loads(response.text)
            return api_response['choices'][0]['message']['content']
        except Exception as e:
            return f"Llama 3 Together API Error: {str(e)}"

