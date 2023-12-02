s1 = s2 = 0
RED, GREEN, BLUE = 12, 13, 14

def parse(line):
	l = line.strip().split(': ')
	most = {'red': 0, 'green': 0, 'blue': 0}

	for s in l[1].split('; '):
		for n, color in [x.split(' ') for x in s.split(', ')]:
			# Store just the maximum cubes per set.
			if (n := int(n)) > most[color]:
				most[color] = n

	return int(l[0].lstrip('Game ')), most
			

with open('input.txt', 'r') as f:
	for line in f.readlines():
		i, m = parse(line)
		if m['red'] <= RED and m['green'] <= GREEN and m['blue'] <= BLUE:
			s1 += i

		s2 += m['red'] * m['green'] * m['blue']

print(s1, s2)
