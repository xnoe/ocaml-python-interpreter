x = input("Your options are: add / subtract")
while not x in ["add", "subtract"]:
	x = input("Your options are: add / subtract")

print("You chose: " + x[0])