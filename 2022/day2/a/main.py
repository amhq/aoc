s = 0
play = [1, 3, 2, 1] # A beats C which beats B which beats A

with open('input.txt', 'r') as f:
	for line in f.readlines():
		p, q = [ord(x)-64 for x in line.strip().split()]
		q -= 23

		if p == q:
			s += 3
		elif q != play[play.index(p)+1]:
			s += 6
		s += q

print(s)

s = 0

with open('input.txt', 'r') as f:
	for line in f.readlines():
		p, o = [ord(x)-64 for x in line.strip().split()]
		o -= 24

		q = p
		if o == 0:
			q = play[play.index(p)+1]
		elif o == 2:
			q = play[play.index(p, 1)-1]

		s += q + (0, 3, 6)[o]

print(s)