import os

def prepare_env(version: str):

    base_directory = "./data/raw/"

    if not os.path.exists(base_directory):
        os.makedirs(base_directory)

    version_directory = base_directory + version + "/"
    if not os.path.exists(version_directory):
        os.makedirs(version_directory)
