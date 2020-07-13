var = 0
def outer():
	global var
	var = 1
	def inner():
		global var
		var = 2
		print("inner:", str(var))
	inner()
	print("outer:", str(var))
outer()
print("main:", str(var))

var = 5
def a():
	def b():
		return var
	return b()

print(str(a()))