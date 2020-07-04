"""Test Color."""
import unittest

from flood_solve.core.color import Color


class ColorTests(unittest.TestCase):
    """Test Color."""

    def test_color_name_format(self):
        for color in Color:
            self.assertEqual(color.value, color.name.lower())

    def test_convert_color_string(self):
        self.assertEqual(Color.get('red'), Color.RED)
        with self.assertRaisesRegex(ValueError, "fake_color is not a known color"):
            Color.get('fake_color')
