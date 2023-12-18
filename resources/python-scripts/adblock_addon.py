import os
import sys
import types
from pathlib import Path

import psycopg2
from adblockparser import AdblockRules
from psycopg2.extras import execute_values
from psycopg2.sql import SQL
from tqdm import tqdm

from request import Request


def main():
    experiment_id = int(sys.argv[1])
    filter_list_name = sys.argv[2]

    addon = AdBlockAddon(filter_list_name)
    addon.match_requests(experiment_id)


class AdBlockAddon:

    def __init__(self, filter_list_name: str):
        self.sql = types.SimpleNamespace()
        self.sql.plugin_schema = "pluginadblock"
        self.sql.request_match_table = "requestmatch"
        self.sql.list_name_table = "filterlist"
        self.conn = psycopg2.connect(host=os.environ['POSTGRES_HOST'] or 'localhost', port=os.environ['HOST_PORT'],
                                     dbname=os.environ['POSTGRES_DB'], user=os.environ['POSTGRES_USER'],
                                     password=os.environ['POSTGRES_PASSWORD'])
        self.cur = self.conn.cursor()

        self.filter_list_name = filter_list_name
        file_path = Path(__file__).with_name(f"{filter_list_name}.txt")
        with open(file_path) as filter_list:
            raw_rules = filter_list.readlines()
        self.rules = AdblockRules(raw_rules, max_mem=2 * 1024 * 1024 * 1024)

    def match_requests(self, experiment_id: int) -> None:
        # get request for experiment
        requests = self._get_experiment_requests(experiment_id)
        print(f"matching {len(requests)} requests using {self.filter_list_name}")
        for r in tqdm(requests):
            path = "" if r.path is None else r.path
            if self.rules.should_block(f"{r.scheme}://{r.host}/{path}"):
                r.match = True
            else:
                r.match = False

        num_hits = len([1 for r in requests if r.match is True])
        print(f"{self.filter_list_name} matches {num_hits} out of {len(requests)} requests "
              f"({(num_hits / len(requests) * 100):.8f} %)")
        self._save_results(requests, )

    def _get_experiment_requests(self, experiment_id: int) -> list[Request]:
        self.cur.execute("SELECT r.id, r.scheme, r.host, r.path "
                         "FROM interfaceanalysis ia "
                         "INNER JOIN trafficcollection tc on tc.analysis = ia.id "
                         "INNER JOIN request r on r.run = tc.id "
                         "WHERE ia.experiment = %s",
                         (experiment_id,))
        self.conn.commit()
        return [Request(r[0], r[1], r[2], r[3]) for r in self.cur.fetchall()]

    def _get_existing_matches(self, experiment_id: int) -> list[tuple[int, bool]]:
        self.cur.execute(SQL(f"SELECT request_id, match "
                             f"FROM {self.sql.request_match_table} "
                             f"WHERE list_id = %s"),
                         (experiment_id,))
        self.conn.commit()
        return self.cur.fetchall()

    def _save_results(self, requests: list[Request]) -> None:
        print("saving the results to the database")
        self._ensure_schema()
        list_id = self._ensure_filter_list(self.filter_list_name)

        match_table = f"{self.sql.plugin_schema}.{self.sql.request_match_table}"
        values = [(r.req_id, list_id, r.match) for r in requests]
        inserted = execute_values(self.cur,
                                  SQL(f"INSERT INTO {match_table} (request_id, list_id, match) "
                                      f"SELECT * FROM (VALUES %s) AS v (rid, lid, result) "
                                      f"WHERE NOT EXISTS "
                                      f"( SELECT request_id FROM {match_table} "
                                      f"  WHERE request_id = v.rid AND list_id = lid) "
                                      f"RETURNING request_id"),
                                  values, fetch=True)
        self.conn.commit()
        insert_count = len(inserted)
        print(f"{insert_count} rows inserted")
        updated = execute_values(self.cur,
                                 SQL(f"UPDATE {match_table} m "
                                     f"SET match = v.match "
                                     f"FROM (SELECT * FROM (VALUES %s) AS w (rid, lid, match)) as v "
                                     f"WHERE m.request_id = v.rid AND m.list_id = v.lid AND m.match != v.match "
                                     f"RETURNING request_id"),
                                 values, fetch=True)
        self.conn.commit()
        update_count = len(updated)
        print(f"{update_count} rows updated")

    def _ensure_schema(self):
        self.cur.execute("SELECT schema_name "
                         "FROM information_schema.schemata "
                         "WHERE schema_name = %s", (self.sql.plugin_schema,))
        self.conn.commit()
        schema_exists_query = self.cur.fetchone()
        if schema_exists_query is None:
            print(f"creating schema '{self.sql.plugin_schema}'")
            self.cur.execute(SQL(f"CREATE SCHEMA {self.sql.plugin_schema}"))
            self.conn.commit()

        # create tables if not exist
        self.cur.execute(SQL(f"CREATE TABLE IF NOT EXISTS {self.sql.plugin_schema}.{self.sql.list_name_table} ("
                             f" id integer NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY ,"
                             f" name varchar NOT NULL"
                             f");"))
        self.conn.commit()

        self.cur.execute(SQL(f"CREATE TABLE IF NOT EXISTS {self.sql.plugin_schema}.{self.sql.request_match_table} ("
                             f" request_id integer NOT NULL REFERENCES public.request(id) "
                             f"  ON UPDATE CASCADE ON DELETE CASCADE ,"
                             f" list_id integer NOT NULL "
                             f"  REFERENCES {self.sql.plugin_schema}.{self.sql.list_name_table}({'id'})"
                             f"  ON UPDATE CASCADE ON DELETE CASCADE ,"
                             f" match boolean NOT NULL ,"
                             f" PRIMARY KEY (request_id, list_id)"
                             f");"))
        self.conn.commit()

    def _ensure_filter_list(self, filter_list_name: str) -> int:
        filter_id = self._get_filter_list_id(filter_list_name)
        if filter_id:
            assert len(filter_id) == 1
            return filter_id[0]
        else:
            self.cur.execute(SQL(f"INSERT INTO {self.sql.plugin_schema}.{self.sql.list_name_table} (name) "
                                 f"VALUES (%s)"), (filter_list_name,))
            self.conn.commit()
            return self._get_filter_list_id(filter_list_name)[0]

    def _get_filter_list_id(self, filter_list_name: str) -> tuple[int]:
        self.cur.execute(SQL(f"SELECT id "
                             f"FROM {self.sql.plugin_schema}.{self.sql.list_name_table} fl "
                             f"WHERE fl.name = %s"), (filter_list_name,))
        self.conn.commit()
        return self.cur.fetchone()


if __name__ == '__main__':
    main()
