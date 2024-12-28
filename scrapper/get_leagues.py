from soccerdata.sofifa import SoFIFA
import pandas as pd

if __name__ == "__main__":
    sofifa = SoFIFA()
    leagues = sofifa.read_leagues().reset_index()
    leagues.to_csv("./data/raw/leagues.csv", index=False)