from abc import ABC, abstractmethod


class Solver(ABC):
    @classmethod
    @abstractmethod
    def solve(cls):
        pass
