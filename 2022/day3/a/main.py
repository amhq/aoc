from string import ascii_letters as letters

s = 0

with open('input.txt', 'r') as f:
	for line in f.readlines():
		s1, s2 = line[:len(line)//2], line[len(line)//2:]
		c = ''.join(set(s1).intersection(set(s2)))
		s += letters.index(c)+1

print(s)

s = 0

with open('input.txt', 'r') as f:
	lines = [l.strip() for l in f.readlines()]
	i = 0

	for line in lines[::3]:
		r1, r2, r3 = set(line), set(lines[i+1]), set(lines[i+2])
		b = ''.join(r1.intersection(r2).intersection(r3))
		i += 3
		s += letters.index(b)+1

print(s)