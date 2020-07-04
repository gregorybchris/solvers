from tile import Tile


class GoalTile(Tile):
    STATUS_SAT = 'sat'
    STATUS_UNSAT = 'unsat'

    ALL_STATUSES = {STATUS_SAT, STATUS_UNSAT}

    @classmethod
    def validate_status(cls, status):
        if status not in cls.ALL_STATUSES:
            raise ValueError(f"Unknown status {status}")

    @classmethod
    def get_status_symbol(cls, status):
        return {
            cls.STATUS_SAT: 'S',
            cls.STATUS_UNSAT: 'U',
        }[status]
