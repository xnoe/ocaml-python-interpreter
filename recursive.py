def rec(x):
	if x == 0:
		print("Fin.")
	else:
		print(str(x))
		print(str(x-1))
		rec(x-1)


rec(5)
