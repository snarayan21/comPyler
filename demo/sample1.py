# This demonstrates nested if-else, variable assignment, and unary + binary + comparison operators 
x = 10
y = 20
z = 15
if x >= z and y >= 0:
    print("In nested loop #1")
    if (y > z and z > y) or (z > 0):
        print("In nested loop #2")
        d = x + y
        if d == 35:
            print("In nested loop #3")
            print("x + y = 35")
        elif d == 30:
            print("In nested loop #3")
            print("x + y = 30")
        else:
            print("In nested loop #3")
            print(d)
elif x == z:
    print("In nested loop #1")
    print("x = z")
else:
    print("In nested loop #1")
    print("x < z")

""" 
Expected output:
In nested loop #1
x < z
"""

