import argparse
import logging
import sys
from enum import Enum
from pathlib import Path

from wordle.genetic.simulation import Simulation

RUN_COMMAND = 'run'
COMMANDS = [RUN_COMMAND]


class LoggingLevel:
    DEBUG = 'DEBUG'
    INFO = 'INFO'
    WARNING = 'WARNING'
    ERROR = 'ERROR'

    ALL = [DEBUG, INFO, WARNING, ERROR]

    @classmethod
    def get_value(cls, level: str):
        level_map = {
            cls.DEBUG: logging.DEBUG,
            cls.INFO: logging.INFO,
            cls.WARNING: logging.WARNING,
            cls.ERROR: logging.ERROR,
        }
        return level_map[level]


logger = logging.getLogger(__name__)


def run() -> None:
    args = parse_args()

    if args.command == RUN_COMMAND:
        test(args.level)
    else:
        raise ValueError(f"Invalid command {args.command}, must be one of {COMMANDS}")


def test(level: str) -> None:
    logging.basicConfig(
        stream=sys.stdout,
        level=LoggingLevel.get_value(level),
        format='%(levelname)s: %(message)s',
    )

    words_filepath = Path(__file__).parent.parent.parent / 'wordle' / 'vocab' / 'vocab.txt'

    n_agents = 100
    n_generations = 40
    n_iterations = 10
    survival_fraction = 0.50
    mutation_rate = 0.05

    simulation = Simulation(n_agents, survival_fraction, mutation_rate, words_filepath)
    simulation.run(n_generations, n_iterations)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(required=True, dest='command', help="Sub-command help.")

    run_parser = subparsers.add_parser(RUN_COMMAND, help="App run subparser.")
    run_parser.add_argument('--level', default=LoggingLevel.ERROR, help="Logging level.", choices=LoggingLevel.ALL)

    return parser.parse_args()
