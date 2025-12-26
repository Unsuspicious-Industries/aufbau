# replace anything like `~/` with the git url of the project
# ~/src/logic/mod.rs -> https://github.com/Unsuspicious-Industries/p7/blob/main/src/logic/mod.rs

GIT_BASE_URL = "https://github.com/Unsuspicious-Industries/p7/blob/main/"

def process(content: str) -> str:
    return content.replace("~/", GIT_BASE_URL)
