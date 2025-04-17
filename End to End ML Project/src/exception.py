import sys
from src.logger import logging

class CustomException(Exception):  
    def __init__(self, error_message, error_detail):
        super().__init__(error_message)
        self.error_message = CustomException.get_detailed_error_message(error_message, error_detail)

    @staticmethod
    def get_detailed_error_message(error_message, error_detail):
        """Extracts error details from sys module and formats them cleanly"""
        _, _, exc_tb = error_detail.exc_info()
        file_name = exc_tb.tb_frame.f_code.co_filename
        line_number = exc_tb.tb_lineno
        return f"[VisionTrackException]\nFile: {file_name}\nLine Number: {line_number}\nIssue: {error_message}"

    def __str__(self):
        return self.error_message
