import logging
from pathlib import Path
from typing import Dict

import numpy as np

from wordle.genetic.agent import Agent
from wordle.solve.checker import evaluate_attempt
from wordle.solve.print_utils import attempt_to_str
from wordle.solve.vocab import get_words

logger = logging.getLogger(__name__)


class Simulation:
    N_ATTEMPTS = 6

    def __init__(self, n_agents: int, survival_fraction: float, mutation_rate: float, words_filepath: Path):
        self.n_agents = n_agents
        self.survival_fraction = survival_fraction
        self.mutation_rate = mutation_rate
        self.words = get_words(words_filepath)

        self.agent_counter = 0
        self.agents: Dict[str, Agent] = {}
        for _ in range(n_agents):
            agent_id = self._get_agent_id()
            agent = Agent.create_random(agent_id, self.N_ATTEMPTS, self.words)
            self.agents[agent_id] = agent

    def _get_agent_id(self) -> str:
        self.agent_counter += 1
        return str(self.agent_counter)

    def run(self, n_generations: int, n_iterations) -> None:
        population_means = []
        population_variances = []
        for generation_number in range(n_generations):
            logger.info(f"Generation: {generation_number}")

            explore_thresholds = []
            first_words = {}
            for agent in list(self.agents.values()):
                explore_thresholds.append(agent.explore_threshold)
                first_word = agent.words[np.argmax(agent.weights[0])]
                if first_word not in first_words:
                    first_words[first_word] = 0
                first_words[first_word] += 1

            logger.info(f"Mean explore threshold: {np.mean(explore_thresholds)}")
            logger.info(f"First word: {sorted(list(first_words.items()), key=lambda x: -x[1])[0]}")

            agent_perf_map = {}
            for agent_number, (agent_id, agent) in enumerate(self.agents.items()):
                logger.debug(f"Agent number: {agent_number} (id={agent_id})")
                if agent_id not in agent_perf_map:
                    agent_perf_map[agent_id] = []

                for iteration_number in range(n_iterations):
                    logger.debug(f"Iteration: {iteration_number}")
                    word_number = np.random.randint(0, len(self.words))
                    target_letters = self.words[word_number]
                    attempt_tiles = None
                    n_attempts_taken = 0
                    for _ in range(self.N_ATTEMPTS):
                        n_attempts_taken += 1
                        attempt_letters = agent.make_attempt(attempt_tiles, n_attempts_taken - 1)

                        if attempt_letters == target_letters:
                            break

                        attempt_tiles = evaluate_attempt(attempt_letters, target_letters)
                        logger.debug(attempt_to_str(attempt_tiles))

                    agent_perf_map[agent.agent_id].append(n_attempts_taken)
                    agent.complete_iteration()

            # Compute agent scores
            agent_score_map: Dict[str, float] = {}
            for agent_id, perf_list in agent_perf_map.items():
                score = np.mean(perf_list)
                agent_score_map[agent_id] = score

            # Compute population score
            all_scores = agent_score_map.values()
            population_score_mean = np.mean(list(all_scores))
            population_score_variance = np.var(list(all_scores))
            population_means.append(population_score_mean)
            population_variances.append(population_score_variance)
            logger.info(f"Population score mean: {population_score_mean}")
            logger.info(f"Population score variance: {population_score_variance}")

            # Keep surviving agents
            new_agents: Dict[str, Agent] = {}
            n_survivors = int(self.survival_fraction * len(self.agents))
            sorted_score_map = sorted(list(agent_score_map.items()), key=lambda x: x[1])
            for agent_id, score in sorted_score_map[:n_survivors]:
                new_agents[agent_id] = self.agents[agent_id]

            # Mutate some surviving agents
            for agent_id, agent in new_agents.items():
                mutation_choice = np.random.rand()
                if mutation_choice < self.mutation_rate:
                    agent.mutate()

            # Add new agents
            while len(new_agents) < self.n_agents:
                agent_a_index = np.random.randint(0, n_survivors)
                agent_b_index = np.random.randint(0, n_survivors)
                agent_a = self.agents[sorted_score_map[agent_a_index][0]]
                agent_b = self.agents[sorted_score_map[agent_b_index][0]]
                agent_id = self._get_agent_id()
                new_agents[agent_id] = Agent.crossover(agent_a, agent_b, agent_id)

            # Update agents
            self.agents = new_agents
