from abc import ABC, abstractmethod


class Tile(ABC):
    def __init__(self, value, status):
        Tile.validate_status(status)
        self._value = value
        self._status = status

    @classmethod
    @abstractmethod
    def validate_status(cls, status):
        pass

    @classmethod
    @abstractmethod
    def get_status_symbol(cls, status):
        pass

    def get_value(self):
        return self._value

    def get_status(self):
        return self._status

    def set_status(self, status):
        self.validate_status(status)
        self._status = status

    def __repr__(self):
        status = self.get_status_symbol(self._status)
        return f"[{self._value}|{status}]"
