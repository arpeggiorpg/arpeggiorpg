from yaml import load, dump


import sys

data = load(open(sys.argv[1]).read())
creatures = data['current_game']['creatures']

for cid in creatures:
    creature = creatures[cid]
    print(creature['name'], creature['initiative'])
    if creature['initiative']['plus'] is not None:
        creature['initiative'] = {
            'Plus': [
                {'Expr': {
                    'num': creature['initiative']['num'],
                    'size': creature['initiative']['size']}},
                {'Expr': {'num': creature['initiative']['plus']['num'],
                          'size': creature['initiative']['plus']['size']}}]}
    else:
        creature['initiative'] = {'Expr': {'num': creature['initiative']['num'],  'size': creature['initiative']['size']}}

new_yaml = dump(data)
with open('new-game.yaml', 'w') as newf:
    newf.write(new_yaml)
