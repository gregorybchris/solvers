"""Flood region colors."""
from enum import auto, unique, Enum


class _ColorName(Enum):
    def _generate_next_value_(self, start, count, last_values):
        return self.lower()


@unique
class Color(_ColorName):
    """Flood region colors."""

    RED = auto()
    ORANGE = auto()
    YELLOW = auto()
    LIME = auto()
    GREEN = auto()
    TEAL = auto()
    BLUE = auto()
    PURPLE = auto()
    MAGENTA = auto()
    PINK = auto()
    WHITE = auto()
    GRAY = auto()
    BLACK = auto()
    TAN = auto()

    @classmethod
    def get(cls, color_string):
        """
        Get an enum color based on a color string.

        :param color_string: The color to convert.
        :return: The enum color.
        """
        for color in cls:
            if color.value == color_string:
                return color
        raise ValueError(f"{color_string} is not a known color")
