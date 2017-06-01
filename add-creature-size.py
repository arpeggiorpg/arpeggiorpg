from yaml import load, dump
import sys

with open(sys.argv[1]) as f:
    yaml = f.read()

data = load(yaml)
for (cid, creature) in data['current_game']['creatures'].items():
    creature['size'] = {"x": 1, "y": 1, "z": 1}

newyaml = dump(data)
with open("out.yaml", "wb") as f:
    f.write(newyaml)
