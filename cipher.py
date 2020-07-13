print ("Welcome to bybb's Python Caesar Cipher Program!")
option = input("Do you want to? 'encode' or 'decode'")
while not option in ["encode", "decode"]:
	option = input("Do you want to? 'encode' or 'decode'")

cipher_text = input("Cipher Text: ")
rot = int(input("Rotation Amount: "))

ciphered_string = ""

if option == "encode":
	for i in cipher_text:
		if i.isalpha():
			if ord(i) > 90:
				ciphered_string = ciphered_string + chr(((ord(i) % 97 + rot) % 26) + 97)
			else:
				ciphered_string = ciphered_string + chr(((ord(i) % 65 + rot) % 26) + 65)
		else:
			ciphered_string = ciphered_string + i
else:
	for i in cipher_text:
		if i.isalpha():
			if ord(i) > 90:
				ciphered_string = ciphered_string + chr(((ord(i) % 97 - rot) % 26) + 97)
			else:
				ciphered_string = ciphered_string + chr(((ord(i) % 65 - rot) % 26) + 65)
		else:
			ciphered_string = ciphered_string + i
print("Your ciphered string is: " + ciphered_string)