from soccerdata.sofifa import SoFIFA
from _prepare_env import prepare_env
import pandas as pd
from concurrent.futures import ThreadPoolExecutor

import os
from dotenv import load_dotenv

load_dotenv()

def get_players(ids: list):
    sofifa = SoFIFA(versions=int(version))
    return sofifa.read_player_ratings(player=ids).reset_index()

if __name__ == "__main__":

    version = os.getenv('SOFIFA_VERSION')
    prepare_env(version)
    path = "./data/raw/"+version+"/"

    players = pd.read_csv(path+"players.csv")
    players_ids = players["player_id"].unique().tolist()

    ids_per_thread = [
        players_ids[:1000],
        players_ids[1000:2000],
        players_ids[2000:3000],
        players_ids[3000:4000],
        players_ids[4000:5000],
        players_ids[5000:],
    ]

    with ThreadPoolExecutor() as executor:
        results = executor.map(get_players, ids_per_thread)

    ratings = pd.concat(results).reset_index()
    ratings.to_csv(path+"ratings.csv", index=False)
