from pathlib import Path
from typing import List


def get_words(words_filepath: Path) -> List[str]:
    with open(words_filepath, 'r') as f:
        return [word.strip() for word in f.readlines()]
