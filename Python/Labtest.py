num= int(input("enter a number: "))

if (num<= 1):
    print(num, "isn't a prime number")
else:
	for i in range(2, num-1, 1):
        	if (num%i==0):
            		print(num, 'is a composite number')
			break
		else:
			print(num, 'is a composite')
