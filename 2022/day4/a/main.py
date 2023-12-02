s1 = s2 = 0

with open('input.txt', 'r') as f:
	for line in f.readlines():
		a1, a2 = [tuple(int(y) for y in x.split('-')) for x in line.strip().split(',')]
		b1 = set(range(a1[0], a1[1]+1))
		b2 = set(range(a2[0], a2[1]+1))
		s1 += bool(b1.issubset(b2) or b2.issubset(b1))
		s2 += bool(len(b1.intersection(b2)))

print(s1, s2)