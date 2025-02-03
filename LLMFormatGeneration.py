import anthropic
import os
from openai import OpenAI
from db import Database


class LLMFormatGeneration:
    def __init__(self, temperature: float):
        self.system_prompt = "You are a software developer who has read standards for several network protocols and file formats and knows the syntax of Data Description Languages like Kaitai Struct, DaeDalus, DFDL, and Zeek Spicy."
        self.temperature = temperature
        # Initialize API clients
        self.openai_client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))
        self.deepseek_client = OpenAI(
            api_key=os.getenv("DEEPSEEK_API_KEY"),
            base_url="https://api.deepseek.com/v1",
        )
        self.together_client = OpenAI(
            api_key=os.getenv("TOGETHER_API_KEY"),
            base_url="https://api.together.xyz/v1",
        )
        self.gemini_client = OpenAI(
            api_key=os.getenv("GOOGLE_API_KEY"),
            base_url="https://generativelanguage.googleapis.com/v1beta/",
        )
        self.anthropic_client = anthropic.Anthropic(
            api_key=os.getenv("ANTHROPIC_API_KEY")
        )
        self.llms = {
            "gemini-1.5-flash": self.call_gpt_api,
            "gpt-4-turbo": self.call_gpt_api,
            "gpt-4o": self.call_gpt_api,
            "claude-3-5-sonnet-20241022": self.call_claude_api,
            "claude-3-5-haiku-20241022": self.call_claude_api,
            "deepseek-chat": self.call_gpt_api,
            "meta-llama/Llama-3.3-70B-Instruct-Turbo": self.call_gpt_api,
        }

        self.api_keys = {
            "gemini-2.0-flash-exp": self.gemini_client,
            "gemini-1.5-flash": self.gemini_client,
            "gpt-4-turbo": self.openai_client,
            "gpt-4o": self.openai_client,
            "deepseek-chat": self.deepseek_client,
            "deepseek-reasoner": self.deepseek_client,
            "meta-llama/Llama-3.3-70B-Instruct-Turbo": self.together_client,
        }

    def call_gpt_api(self, model: str, messages):
        """Call OpenAI's GPT API"""
        api_client = self.api_keys[model]
        try:
            response = api_client.chat.completions.create(
                model=model,
                max_tokens=4096,
                messages=messages,
                temperature=self.temperature,
            )
            return response.choices[0].message
        except Exception as e:
            import sys

            print(f"{model} Error: {str(e)}")
            return f"{model} Error: {str(e)}"

    def call_claude_api(self, model: str, messages):
        """Call Anthropic's Claude API"""
        try:
            response = self.anthropic_client.messages.create(
                model=model,
                max_tokens=4096,
                system=self.system_prompt,
                messages=messages,
                temperature=self.temperature,
            )
            return response.content[0]
        except Exception as e:
            return f"Claude API Error: {str(e)}"
