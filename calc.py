NUMBER_TOKEN = 0
PLUS_TOKEN = 1
MINUS_TOKEN = 2
TIMES_TOKEN = 3
DIVIDE_TOKEN = 4
LEFT_BRACKET_TOKEN = 5
RIGHT_BRACKET_TOKEN = 6
WHITESPACE_TOKEN = 7
EOF_TOKEN = 8

program = input("Please enter an expression to evaluate: ")
tokens = []

i = 0
while i < len(program):
	if program[i].isdigit():
		tmp_number = 0
		while i < len(program) and program[i].isdigit() :
			tmp_number *= 10
			tmp_number += ord(program[i]) % 48
			i += 1
		tokens.append([NUMBER_TOKEN, tmp_number])
		i -= 1
	elif program[i] == "+":
		tokens.append([PLUS_TOKEN])
	elif program[i] == "-":
		tokens.append([MINUS_TOKEN])
	elif program[i] == "*":
		tokens.append([TIMES_TOKEN])
	elif program[i] == "/":
		tokens.append([DIVIDE_TOKEN])
	elif program[i] == "(":
		tokens.append([LEFT_BRACKET_TOKEN])
	elif program[i] == ")":
		tokens.append([RIGHT_BRACKET_TOKEN])
	elif program[i] == " ":
		tokens.append([WHITESPACE_TOKEN])
	else:
		print("Invalid charatcer: " + i)
	i += 1
tokens.append([EOF_TOKEN])

ast = []
NUMBER_AST = 0
PLUS_AST = 1
MINUS_AST = 2
TIMES_AST = 3
DIVIDE_AST = 4
NEGATE_AST = 5

index = 0

print("here")

def current ():
	return (tokens[index])[0]

def eat(token):
	global index
	if current() == token:
		index += 1
	else:
		print("Invalid token")

def eat_whitespace():
	while current() == WHITESPACE_TOKEN:
		eat (WHITESPACE_TOKEN)

def factor():
	node = []
	eat_whitespace()
	if current() == NUMBER_TOKEN:
		node = [NUMBER_AST, tokens[index][1]]
		eat (NUMBER_TOKEN)
	elif current() == LEFT_BRACKET_TOKEN:
		eat (LEFT_BRACKET_TOKEN)
		node = expr()
		eat (RIGHT_BRACKET_TOKEN)
	else:
		eat (MINUS_TOKEN)
		node = [NEGATE_AST, factor()]
	eat_whitespace()
	return node
def term():
	node = factor()
	while current() in [TIMES_TOKEN, DIVIDE_TOKEN]:
		if current () == TIMES_TOKEN:
			eat (TIMES_TOKEN)
			node = [TIMES_AST, node, factor()]
		else:
			eat (DIVIDE_TOKEN)
			node = [DIVIDE_AST, node, factor()]
	return node
def expr():
	node = term()
	while current() in [PLUS_TOKEN, MINUS_TOKEN]:
		if current() == PLUS_TOKEN:
			eat (PLUS_TOKEN)
			node = [PLUS_AST, node, term()]
		else:
			eat (MINUS_TOKEN)
			node = [MINUS_AST, node, term()]
	return node

def interpret (ast):
	if ast[0] == NUMBER_AST:
		return ast[1]
	elif ast[0] == PLUS_AST:
		return interpret(ast[1]) + interpret(ast[2])
	elif ast[0] == MINUS_AST:
		return interpret(ast[1]) - interpret(ast[2])
	elif ast[0] == TIMES_AST:
		return interpret(ast[1]) * interpret(ast[2])
	elif ast[0] == DIVIDE_AST:
		return interpret(ast[1]) / interpret(ast[2])
	elif ast[0] == NEGATE_AST:
		return -interpret(ast[1])

print("Your answer: " + str(interpret(expr())))
