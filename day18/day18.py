import re

dir_map = {'U': (0, 1), 'D': (0, -1), 'L': (-1, 0), 'R': (1, 0),
        '3': (0, 1), '1': (0, -1), '2': (-1, 0), '0': (1, 0)}

def read_data():
    pat = re.compile(r'(\w)\s(\d+)...([\w]{6})')
    with open('example.txt') as f:
        return map(lambda x: pat.match(x).groups(), f.read().splitlines())

def shoelace(vertices):
    shoelace = 0
    for i in range(len(vertices) - 1):
        shoelace += vertices[i][0] * vertices[i + 1][1] - \
            vertices[i + 1][0] * vertices[i][1]
    return abs(shoelace) // 2

def part1generator():
    yield 0, 0, 0
    for uplr, steps, _ in read_data():
        yield (*dir_map[uplr], int(steps))

def part2generator():
    yield 0, 0, 0
    for _, _, code in read_data():
        yield (*dir_map[code[-1]], int(code[:-1], 16))

def calcarea(pgen):
    vertices = [(0, 0)]
    perimeter = 0
    for dx, dy, steps in pgen():
        vertices.append((vertices[-1][0] + dx * steps,
                        vertices[-1][1] + dy * steps))
        perimeter += steps
    print("perimeter:", perimeter)

    print(shoelace(vertices) + perimeter // 2 + 1)

calcarea(part1generator)
calcarea(part2generator)