def hello():
    x = 4
    y = x + 4
    x = x + 2

    print(x)
    print("Hello World!")

    for i in range(4):
        x = x + i

def james():
    hello()
    print("Called hello from James")

james()
