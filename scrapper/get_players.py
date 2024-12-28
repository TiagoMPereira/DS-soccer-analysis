from soccerdata.sofifa import SoFIFA
from _prepare_env import prepare_env

import os
from dotenv import load_dotenv

load_dotenv()

if __name__ == "__main__":

    version = os.getenv('SOFIFA_VERSION')
    prepare_env(version)
    path = "./data/raw/"+version+"/"

    sofifa = SoFIFA(versions=int(version))
    players = sofifa.read_players().reset_index()
    players.to_csv(path+"players.csv", index=False)
