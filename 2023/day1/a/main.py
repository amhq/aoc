s = 0

with open('sample1.txt', 'r') as f:
	for line in f.readlines():
		c = ''.join([r for r in line if r.isdigit()])
		s += int(c[0] + c[-1])

print(s)

s = 0

numbers = {
	'one': 1,
	'two': 2,
	'three': 3,
	'four': 4,
	'five': 5,
	'six': 6,
	'seven': 7,
	'eight': 8,
	'nine': 9
}

def find_number(line, invert=False):
	if (d := line[0]).isdigit():
		return int(d)

	for n in numbers.keys():
		if line.startswith(n[::-1 if invert else 1]):
			return numbers[n]

	return -1

with open('input.txt', 'r') as f:
	for line in f.readlines():
		while (left := find_number(line)) == -1:
			line = line[1:]

		while (right := find_number(line[::-1], True)) == -1:
			line = line[:-1]

		s += left * 10 + right

print(s)