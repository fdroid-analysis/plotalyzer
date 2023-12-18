import os
import types

import psycopg2
from psycopg2 import sql
from psycopg2.extras import execute_values
from psycopg2.sql import SQL

from private_data import PrivateData
from request import NormalizedRequest
from request import Request


class Database:
    def __init__(self):
        self.sql = types.SimpleNamespace()
        self.conn = psycopg2.connect(host=os.environ['POSTGRES_HOST'] or 'localhost', port=os.environ['HOST_PORT'],
                                     dbname=os.environ['POSTGRES_DB'], user=os.environ['POSTGRES_USER'],
                                     password=os.environ['POSTGRES_PASSWORD'])
        self.cur = self.conn.cursor()

    def _get_requests(self, query: str, params) -> list[Request]:
        self.cur.execute(query, params)
        self.conn.commit()
        return [Request(r[0], r[1], r[2], r[3], r[4]) for r in self.cur.fetchall()]

    def get_experiment_requests(self, experiment_id: int) -> list[Request]:
        self.cur.execute("SELECT r.id, r.scheme, r.host, r.path, r.content "
                         "FROM interfaceanalysis ia "
                         "INNER JOIN trafficcollection tc on tc.analysis = ia.id "
                         "INNER JOIN request r on r.run = tc.id "
                         "WHERE ia.experiment = %s "
                         "AND r.error IS Null "
                         "AND ia.success IS true;",
                         (experiment_id,))
        self.conn.commit()
        return [Request(r[0], r[1], r[2], r[3], r[4]) for r in self.cur.fetchall()]

    def get_experiment_matched_requests(self, experiment_id: int) -> list[Request]:
        self.cur.execute("SELECT DISTINCT r.id, r.scheme, r.host, r.path, r.content "
                         "FROM interfaceanalysis ia "
                         "INNER JOIN trafficcollection tc on tc.analysis = ia.id "
                         "INNER JOIN request r on r.run = tc.id "
                         "INNER JOIN pluginadblock.requestmatch rm on rm.request_id = r.id "
                         "WHERE ia.experiment = %s "
                         "AND r.error IS Null "
                         "AND ia.success IS true "
                         "AND rm.match IS true;",
                         (experiment_id,))
        self.conn.commit()
        return [Request(r[0], r[1], r[2], r[3], r[4]) for r in self.cur.fetchall()]

    def get_experiment_app_requests(self, experiment_id: int, only_apps: list[str]) -> list[Request]:
        values = [sql.Literal(app_id) for app_id in only_apps]
        query = (SQL("""SELECT DISTINCT r.id, r.scheme, r.host, r.path, r.content 
                    FROM interfaceanalysis ia 
                    INNER JOIN trafficcollection tc on tc.analysis = ia.id 
                    INNER JOIN request r on r.run = tc.id 
                    WHERE ia.experiment = {experiment}
                    AND r.error IS Null 
                    AND ia.success IS true 
                    AND ia.app_id IN ({apps});""")
                 .format(experiment=sql.Literal(experiment_id), apps=SQL(', ').join(values)))
        self.cur.execute(query)
        requests = self.cur.fetchall()
        return [Request(r[0], r[1], r[2], r[3], r[4]) for r in requests]

    def insert_normalized_requests(self, normalized_requests: list[NormalizedRequest]) -> bool:
        values = [(r.request_id, r.normalized_id) for r in normalized_requests]
        inserted = execute_values(self.cur,
                                  SQL(f"INSERT INTO request_normalized (request_id, normalized_request_id) "
                                      f"SELECT * FROM (VALUES %s) AS v (rid, nid) "
                                      f"WHERE NOT EXISTS "
                                      f"( SELECT request_id FROM request_normalized "
                                      f"  WHERE request_id = v.rid) "
                                      f"RETURNING request_id"),
                                  values, fetch=True)
        self.conn.commit()
        return len(values) == len(inserted)

    def get_normalized_requests(self) -> list[NormalizedRequest]:
        self.cur.execute("SELECT n.request_id, n.normalized_request_id "
                         "FROM request_normalized n;")
        self.conn.commit()
        return [NormalizedRequest(n[0], n[1]) for n in self.cur.fetchall()]

    def set_normalized_request_analyzed(self, request_id: int, source: str, model):
        values = [(request_id, source, model)]
        inserted = execute_values(self.cur,
                                  SQL("INSERT INTO pluginadblock.request_llm_analyzed (request_id, source, model) "
                                      "SELECT * FROM (VALUES %s) AS v (rid, source, model) "
                                      "WHERE NOT EXISTS "
                                      "( SELECT request_id FROM pluginadblock.request_llm_analyzed "
                                      "  WHERE request_id = v.rid AND source = v.source and model = v.model) "
                                      "RETURNING request_id;"),
                                  values, fetch=True)
        self.conn.commit()
        return len(inserted) == len(values)

    def get_normalized_request_ids_analyzed(self, source: str, model: str) -> list[int]:
        self.cur.execute("SELECT request_id "
                         "FROM pluginadblock.request_llm_analyzed "
                         "WHERE source = %s "
                         "  AND model = %s;", (source, model))
        self.conn.commit()
        request_ids = [r[0] for r in self.cur.fetchall()]
        return request_ids

    def insert_private_data(self, request: Request, private_data: list[PrivateData]) -> bool:
        values = [(request.req_id, pd.category, pd.key, pd.value, pd.source, pd.model, pd.count) for pd in private_data]
        inserted = execute_values(
            self.cur,
            SQL(f"INSERT INTO private_data (request_id, category, key, value, source, model, times) "
                f"SELECT * FROM (VALUES %s) AS v (rid, category, key, value, source, model, times) "
                f"WHERE NOT EXISTS "
                f"( SELECT request_id FROM private_data "
                f"  WHERE request_id = v.rid AND category = v.category "
                f"    AND key = v.key AND value = v.value AND source = v.source"
                f"    AND model = v.model) "
                f"RETURNING request_id"),
            values, fetch=True)
        self.conn.commit()
        return len(values) == len(inserted)

    def get_private_data(self) -> dict[int, list[PrivateData]]:
        self.cur.execute("SELECT p.request_id, p.category, p.key, p.value, p.source, p.model, p.times "
                         "FROM private_data p;")
        self.conn.commit()
        entries = self.cur.fetchall()
        private_data: dict[int, list[PrivateData]] = dict()
        for entry in entries:
            request_id = entry[0]
            data = PrivateData(entry[1], entry[2], entry[3], entry[4], entry[5], entry[6])
            if request_id not in private_data:
                private_data[request_id] = list()
            private_data[request_id].append(data)
        return private_data
