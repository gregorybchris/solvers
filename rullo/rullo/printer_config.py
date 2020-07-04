import json

from schema import Optional, Or, Schema


class PrinterConfig:
    KEY_COLOR_BLUE = 'blue'
    KEY_COLOR_CYAN = 'cyan'
    KEY_COLOR_GREEN = 'green'
    KEY_COLOR_ORANGE = 'orange'
    KEY_COLOR_RED = 'red'
    KEY_COLOR_RESET = 'reset'

    ALL_KEY_COLOR = {
        KEY_COLOR_BLUE,
        KEY_COLOR_CYAN,
        KEY_COLOR_GREEN,
        KEY_COLOR_ORANGE,
        KEY_COLOR_RED
    }

    KEY_STATUS_MAYBE = 'maybe'
    KEY_STATUS_YES = 'yes'
    KEY_STATUS_NO = 'no'
    KEY_STATUS_SAT = 'sat'
    KEY_STATUS_UNSAT = 'unsat'

    ALL_KEY_STATUS = {
        KEY_STATUS_MAYBE,
        KEY_STATUS_YES,
        KEY_STATUS_NO,
        KEY_STATUS_SAT,
        KEY_STATUS_UNSAT,
    }

    KEY_COLORS = 'colors'

    CONFIG_SCHEMA = Schema({
        Optional(KEY_COLORS): {
            Or(*ALL_KEY_STATUS): Or(*ALL_KEY_COLOR)
        },
    })

    def __init__(self, config_dict=None):
        if config_dict is None:
            config_dict = dict()
        PrinterConfig._parse(self, config_dict)
        print(self.colors)

    @classmethod
    def _parse(cls, self, config):
        self.colors = cls._get([cls.KEY_COLORS], config)

    @classmethod
    def _get(cls, path, config):
        if len(path) < 1:
            raise ValueError("Path must have at least one value")

        while len(path) > 0:
            if path[0] not in config:
                return None
            config = config[path[0]]
            path = path[1:]
        return config

    @classmethod
    def from_file(cls, filename):
        with open(filename, 'r') as f:
            config_dict = json.load(f)
        cls.CONFIG_SCHEMA.validate(config_dict)
        return PrinterConfig(config_dict)
