from string import digits
from functools import reduce

s0 = s1 = 0
skip = digits + '.'

def check(lines, i, j):
	for i1 in range(i-1, i+2):
		for j1 in range(j-1, j+2):
			if lines[i1][j1] not in skip:
				return True
	return False

def check2(lines, i, j):
	for i1 in range(i-1, i+2):
		for j1 in range(j-1, j+2):
			if lines[i1][j1] == '*':
				return (i1, j1)
	return ()

with open('input.txt', 'r') as f:
	lines = [f'.{l.strip()}.' for l in f.readlines()]
	lines = ['.' * len(lines[0])] + lines + ['.' * len(lines[0])]
	gears = {}

	for i, line in enumerate(lines):
		n, add, gadd = '', False, ()

		for j, c in enumerate(line):
			if c.isnumeric():
				n += c
				add = add or check(lines, i, j)
				gadd = check2(lines, i, j) if gadd == () else gadd
				continue

			if add:
				s0 += int(n)

			if gadd:
				if gadd in gears:
					gears[gadd].append(int(n))
				else:
					gears[gadd] = [int(n)]
				
			n, add, gadd = '', False, ()

	for x in gears.values():
		s1 += x[0] * x[1] if len(x) == 2 else 0

print(s0, s1)
