class Request:

    def __init__(self, req_id: str, scheme: str, host: str, path: str, content: str = ""):
        self.req_id = int(req_id)
        self.scheme = scheme,
        self.host = host
        self.path = path
        self.content = "" if content is None else content
        self.match = None

    def __eq__(self, other):
        return self.req_id == other.req_id and self.match == other.match and self.same_values(other)

    def __hash__(self):
        return hash((self.req_id, self.scheme, self.host, self.path, self.content, self.match))

    def __repr__(self):
        return f"Request({self.req_id}: {self.scheme[0]}://{self.host}{self.path}: {self.content[:50]} - {self.match})"

    def same_values(self, other) -> bool:
        return self.scheme == other.scheme and self.host == other.host and self.path == other.path and self.content == other.content


class NormalizedRequest:
    def __init__(self, request_id: int, normalized_id: int):
        self.request_id = request_id
        self.normalized_id = normalized_id

    def __repr__(self):
        return f"NormalizedRequest({self.request_id}, {self.normalized_id})"
