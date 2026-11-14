"""Optional development fixture generator. Requires treys==0.1.8.
Run from the repository root; writes text CSV, never R datasets.
"""
import csv
import random
from pathlib import Path
from treys import Card, Evaluator

rng = random.Random(20261114)
pack = [rank + suit for rank in '23456789TJQKA' for suit in 'shdc']
evaluator = Evaluator()
rows = []
for count in (5, 6, 7):
    for _ in range(60):
        left, right = rng.sample(pack, count), rng.sample(pack, count)
        lhs = evaluator.evaluate([], [Card.new(c) for c in left])
        rhs = evaluator.evaluate([], [Card.new(c) for c in right])
        rows.append([' '.join(left), ' '.join(right), lhs, rhs, (rhs > lhs) - (rhs < lhs)])
path = Path('tests/testthat/fixtures/treys-rankings.csv')
with path.open('w', newline='') as handle:
    writer = csv.writer(handle, lineterminator='\n')
    writer.writerow(['left', 'right', 'treys_left', 'treys_right', 'expected'])
    writer.writerows(rows)
print(f'Wrote {len(rows)} independently evaluated comparisons (Treys 0.1.8).')
