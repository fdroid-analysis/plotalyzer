import sys

from tqdm import tqdm

from database import Database
from llm import LLM
from private_data import PrivateData, deduplicate_private_data
from request import NormalizedRequest
from request import Request
from llm import (gpt3, gpt4)

db = Database()
match_modes = ("tracking", "all")


def main():
    experiment_id = int(sys.argv[1])
    batch_size = int(sys.argv[2])
    match_mode = sys.argv[3]
    source = sys.argv[4]
    model = gpt4
    only_file = sys.argv[5]
    if match_mode not in match_modes:
        print(f"third argument must match one of these options: {match_modes}", file=sys.stderr)
        return
    if only_file != "none":
        only_apps = read_only_file(only_file)
    else:
        only_apps = list()
    analyze_experiment(experiment_id, batch_size, match_mode, source, model, only_apps)


def read_only_file(only_file_path: str) -> list[str]:
    with open(only_file_path, "r") as f:
        lines = f.readlines()
        apps = [line[:-1] if line[-1] == '\n' else line for line in lines]
    return apps


def normalize_requests(requests: list[Request]) -> dict[Request, list[Request]]:
    normals: dict[Request, list[Request]] = dict()
    for request in requests:
        exists = False
        for normal in normals:
            if normal.same_values(request):
                normals[normal].append(request)
                exists = True
                break
        if not exists:
            normals[request] = list()

    return normals


def save_normalized_requests(normalized_requests: dict[Request, list[Request]]):
    requests_normalized: list[NormalizedRequest] = []
    for normal in normalized_requests.keys():
        requests_normalized.append(NormalizedRequest(normal.req_id, normal.req_id))
        for request in normalized_requests[normal]:
            requests_normalized.append(NormalizedRequest(request.req_id, normal.req_id))
    result = db.insert_normalized_requests(requests_normalized)
    print(f"saved all request normalizations: {result}")


def get_normals_to_analyze(normals: list[Request], source: str, model: str) -> list[Request]:
    analyzed_normal_request_ids = db.get_normalized_request_ids_analyzed(source, model)
    remaining_normals = [n for n in normals if n.req_id not in analyzed_normal_request_ids]
    return remaining_normals


def analyze_experiment(
        experiment_id: int, batch_size: int,
        match_mode: str, source: str, model: str,
        only_apps: list[str]):

    print(f"analyzing {match_mode} requests from experiment {experiment_id} "
          f"with batch size {batch_size}, model {model}")
    # get requests for experiment (rid, query, content)
    if match_mode == match_modes[0]:
        requests = db.get_experiment_matched_requests(experiment_id)
    elif len(only_apps) > 0:
        requests = db.get_experiment_app_requests(experiment_id, only_apps)
    else:
        requests = db.get_experiment_requests(experiment_id)
    print(f"number of requests: {len(requests)}")
    # normalize requests (host, path, content)
    normalized_requests = normalize_requests(requests)
    print(f"number of normalized requests: {len(normalized_requests)}")
    save_normalized_requests(normalized_requests)
    # exclude analyzed requests
    normals = list(normalized_requests.keys())
    normals_to_analyze = get_normals_to_analyze(normals, source, model)
    print(f"remaining number of normals to analyze: {len(normals_to_analyze)}")
    normals_batch = normals_to_analyze[:batch_size]
    print([n.req_id for n in normals_batch])
    # ask chatgpt for private data in query (if exists) or content
    llm = LLM(model, source)
    for normalized_request in tqdm(normals_batch):
        private_data: list[PrivateData] = list()
        query_split = normalized_request.path.split("?")[1:]
        if len(query_split) == 1 and query_split[0] != "":
            query = query_split[0]
            print(query)
            private_data += llm.get_private_data(query)
        else:
            print(f"{normalized_request.req_id}: no query string")
        if normalized_request.content != "":
            private_data += llm.get_private_data(normalized_request.content)
        else:
            print(f"{normalized_request.req_id}: no content")
        # insert responses for request in separate table
        print(f"found {len(private_data)} raw pairs of private data")
        private_data = deduplicate_private_data(private_data)
        print(f"after normalization there are {len(private_data)} pairs of private data")
        db.insert_private_data(normalized_request, private_data)
        db.set_normalized_request_analyzed(normalized_request.req_id, source, model)


if __name__ == '__main__':
    main()
