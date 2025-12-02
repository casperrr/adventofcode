import dotenv
import os
import requests

# Read in input
dotenv.load_dotenv()
aoc_session = os.getenv('AOC_SESSION')

response = requests.get(
    'https://adventofcode.com/2025/day/1/input',
    cookies={'session': aoc_session}
)
# print(response.text)

input_data = response.text.strip().split('\n')

def parse(data):
    direction, value = data[0], int(data[1:])
    return -int(value) if direction == 'L' else int(value)

turns = [parse(line) for line in input_data]

# print(turns)

def turn(pos, turn):
    return (pos + turn) % 100

positions = [50]
for i, t in enumerate(turns):
    positions.append(turn(positions[-1], t))

part1 = len(list(filter(lambda x: x == 0, positions)))

print(part1)