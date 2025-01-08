import requests
import google.generativeai as genai
import anthropic
import os
from openai import OpenAI

class LLMFormatGeneration:
    def __init__(self):
        # Initialize API clients
        self.openai_client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        self.anthropic_client = anthropic.Anthropic(api_key=os.getenv('ANTHROPIC_API_KEY'))
        self.deepseek_api_key = os.getenv('DEEPSEEK_API_KEY')
        self.grok_api_key = os.getenv('XAI_API_KEY')
        genai.configure(api_key=os.getenv('GOOGLE_API_KEY'))
        self.llms = {
            "gemini-pro": self.call_gemini_api,
            "gpt-4-turbo": self.call_gpt_api,
            "claude-3-opus-20240229": self.call_claude_api,
            "deepseek": self.call_deepseek_api
        }

    def call_grok_api(self, query: str, model: str) -> str:
        """Call XAI's Grok model"""
        try:
            response = requests.post(
                "https://api.x.ai/v1/chat/completions",
                headers={
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {self.grok_api_key}"
                },
                json={
                    "model": "grok-beta",
                    "messages": [
                        {"role": "system", "content": "You are a software developer who has read standards for several network protocols and file formats and knows the syntax of Data Description Languages like Kaitai Struct, DaeDalus, DFDL, and Parsley"},
                        {"role": "user", "content": query}
                    ],
                    "stream": False,
                    "temperature": 0.4,
                    "max_tokens": 4096
                }
            )
            return response.json()['choices'][0]['message']['content']
        except Exception as e:
            return f"Grok API Error: {str(e)}"

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