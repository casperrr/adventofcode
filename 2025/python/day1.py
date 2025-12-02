import dotenv
import os
import requests
from itertools import accumulate

# Read in input
dotenv.load_dotenv()
aoc_session = os.getenv('AOC_SESSION')

data = requests.get(
    'https://adventofcode.com/2025/day/1/input',
    cookies={'session': aoc_session}
).text.strip().split('\n')

# Part 1 - count number of times a turn lands on 0
turns = [-int(line[1:]) if line[0] == 'L' else int(line[1:]) for line in data]
d1p1 = sum(p%100==0 for p in accumulate(turns, initial=50))
print(f"Part 1: {d1p1}")

# Part 2 - count number of times a turn crosses 0
positions = list(accumulate(turns, lambda p, t: (p+t)%100, initial=50))
d1p2 = sum((p+t)//100 if t>0 else (p+t)//-100-p//-100 for p, t in zip(positions, turns))
print(f"Part 2: {d1p2}")