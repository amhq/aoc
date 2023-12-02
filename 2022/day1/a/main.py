s = 0

with open('input.txt', 'r') as f:
	cal = [sum([int(c) for c in e.splitlines()]) for e in f.read().split('\n\n')]
	print(max(cal), sum(sorted(cal)[-3:]))