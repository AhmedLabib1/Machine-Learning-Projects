import logging
import os
from datetime import datetime

# Define log file name with timestamp
log_file = f"{datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.log"

# Define logs directory path
logs_dir = os.path.join(os.getcwd(), "ML Engine", "src", "logs")
os.makedirs(logs_dir, exist_ok = True)

# Full path for the log file
LOG_FILE_PATH = os.path.join(logs_dir, log_file)

# Configure logging
logging.basicConfig(
    filename=LOG_FILE_PATH,  # Specifies the log file where logs will be written
    level=logging.INFO,  # Sets the logging level (INFO and above messages will be logged)
    format="%(asctime)s - %(levelname)s - %(message)s",  # Defines the log message format
)