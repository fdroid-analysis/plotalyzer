class PrivateData:
    def __init__(self, category: str, key: str, value: str, source: str, model: str, count: int = 1):
        self.category = category
        self.key = key
        self.value = value
        self.source = source
        self.model = model
        self.count = count

    def __eq__(self, other):
        return self.same_value(other) and self.count == other.count

    def __hash__(self):
        return hash((self.category, self.key, self.value, self.source, self.model, self.count))

    def __repr__(self):
        return f"PrivateData({self.category}: {self.key}, {self.value}, {self.source}, {self.model} - {self.count})"

    def same_value(self, other):
        return (self.category == other.category and self.key == other.key
                and self.value == other.value and self.source == other.source and self.model == other.model)


def deduplicate_private_data(private_data: list[PrivateData]) -> list[PrivateData]:
    normalized_data: set[PrivateData] = set()
    for date in private_data:
        data = PrivateData(date.category, date.key, date.value, date.source, date.model, private_data.count(date))
        normalized_data.add(data)
    return list(normalized_data)
