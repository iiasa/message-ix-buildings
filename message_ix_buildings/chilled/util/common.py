import logging
import sys
from pathlib import Path


def get_project_root() -> Path:
    return Path(__file__).parent.parent.parent


def get_logger(name: str):
    log = logging.getLogger(name)
    log.setLevel(logging.INFO)

    # configure the handler and formatter as needed
    handler = logging.StreamHandler(sys.stdout)
    formatter = logging.Formatter("%(name)s %(asctime)s %(levelname)s %(message)s")

    # add formatter to the handler
    handler.setFormatter(formatter)

    # add handler to the logger
    log.addHandler(handler)

    return log
