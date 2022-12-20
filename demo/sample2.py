# This demonstrates nested a basic function, local parameters, recursion, and binary logical operators.
def factorial(n):     
    # single line to find factorial
    if n == 1 or n == 0:
        return 1
    else:
        return n * factorial(n - 1)

print(factorial(5))

""" 
Expected output:
120
"""