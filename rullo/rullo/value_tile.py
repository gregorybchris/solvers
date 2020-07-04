from tile import Tile


class ValueTile(Tile):
    STATUS_YES = 'yes'
    STATUS_NO = 'no'
    STATUS_MAYBE = 'maybe'

    ALL_STATUSES = {STATUS_YES, STATUS_NO, STATUS_MAYBE}

    @classmethod
    def validate_status(cls, status):
        if status not in cls.ALL_STATUSES:
            raise ValueError(f"Unknown status {status}")

    @classmethod
    def get_status_symbol(cls, status):
        return {
            cls.STATUS_YES: 'Y',
            cls.STATUS_NO: 'N',
            cls.STATUS_MAYBE: 'M',
        }[status]
