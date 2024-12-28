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
    teams = sofifa.read_teams().reset_index()
    teams.to_csv(path+"teams.csv", index=False)
