import logging
from typing import List, Optional

import numpy as np

from wordle.solve.tile import Tile
from wordle.solve.checker import get_valid_words

logger = logging.getLogger(__name__)


class Agent:
    def __init__(
        self,
        agent_id: str,
        weights: np.ndarray,
        words: List[str],
        explore_threshold: int,
    ):
        self.agent_id = agent_id
        self.weights = weights
        self.words = words
        self.iteration_words = words
        self.explore_threshold = explore_threshold

    def complete_iteration(self):
        self.iteration_words = self.words

    def make_attempt(self, last_attempt_tiles: Optional[List[List[Tile]]], attempt_number: int) -> str:
        if last_attempt_tiles is not None:
            self.iteration_words = get_valid_words([last_attempt_tiles], self.iteration_words)
        if len(self.iteration_words) > self.explore_threshold:
            logger.debug(f"Valid words left: {len(self.iteration_words)} (explore, thold = {self.explore_threshold})")
            weights = self.weights[attempt_number]
            return self.words[np.argmax(weights)]
        else:
            logger.debug(f"Valid words left: {len(self.iteration_words)} (exploit), thold = {self.explore_threshold})")
            return self.iteration_words[0]

    def mutate(self):
        self.weights = np.random.rand(*self.weights.shape)
        self.explore_threshold = np.random.randint(0, len(self.words))

    @classmethod
    def create_random(cls, agent_id: str, n_attempts: int, words: List[str]) -> 'Agent':
        weights = np.random.rand(n_attempts, len(words))
        explore_threshold = np.random.randint(0, len(words))
        return cls(agent_id, weights, words, explore_threshold)

    @classmethod
    def crossover(cls, agent_a: 'Agent', agent_b: 'Agent', agent_id: str) -> 'Agent':
        mask = np.random.randint(0, 2, agent_a.weights.shape)
        weights = mask * agent_a.weights + (1 - mask) * agent_b.weights
        threshold_choice = np.random.randint(0, 2)
        explore_threshold = agent_a.explore_threshold if threshold_choice == 0 else agent_b.explore_threshold
        return cls(agent_id, weights, agent_a.words, explore_threshold)
