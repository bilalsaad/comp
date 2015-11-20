import random
import sys

def gen_op():
	dict = {0: '+', 1: '-', 2: '*',3: '/'}
	return dict[random.randrange(0, 4, 1)]

def gen_exp(rng):
	cont = random.randrange(0, rng, 1)
	level = random.randrange(2, 5, 1)

	# gen natural without 0 (cuz of division)
	if cont == 0:
		rnd = random.randrange(-100, 100, 1)
		if rnd == 0 : rnd = 1
		return str(rnd)
	else:
		bunch = "(" + gen_op()
		for x in range(0, level):
			bunch += " " + gen_exp(rng - 1)
		bunch += ")"
		return bunch
	
def main():
	# default insanity level
	insanity_level = 7
	if len(sys.argv) > 1 : insanity_level = int(sys.argv[1])
	print (gen_exp(insanity_level))

if __name__ == "__main__":
	main()	