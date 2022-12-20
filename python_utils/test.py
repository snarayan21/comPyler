import dis


def hello():
    x = 3
    y = 2
    for i in range(x): 
        if i == 2: 
            print(x)
        else: 
            z = 3
    print(y)


if __name__ == "__main__":
    hello()
    print(dis.dis(hello))


'''

0,8
8,9
9,14
14,20
20,23
23,29

[InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},
InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},
InstructionWInd {opc = STORE_NAME, ind = Index 3, a = Just (Arg 1)},
InstructionWInd {opc = LOAD_NAME, ind = Index 4, a = Just (Arg 2)},
InstructionWInd {opc = LOAD_NAME, ind = Index 5, a = Just (Arg 0)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 6, a = Just (Arg 1)},
InstructionWInd {opc = GET_ITER, ind = Index 7, a = Just (Arg 1)},

InstructionWInd {opc = FOR_ITER, ind = Index 8, a = Just (Arg 14)},

InstructionWInd {opc = STORE_NAME, ind = Index 9, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_NAME, ind = Index 10, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_CONST, ind = Index 11, a = Just (Arg 1)},
InstructionWInd {opc = COMPARE_OP, ind = Index 12, a = Just (Arg 2)},
InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 13, a = Just (Arg 20)},

InstructionWInd {opc = LOAD_NAME, ind = Index 14, a = Just (Arg 4)},
InstructionWInd {opc = LOAD_NAME, ind = Index 15, a = Just (Arg 0)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 16, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 17, a = Nothing},
InstructionWInd {opc = LOAD_CONST, ind = Index 18, a = Just (Arg 2)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 19, a = Nothing},

InstructionWInd {opc = LOAD_CONST, ind = Index 20, a = Just (Arg 0)},
InstructionWInd {opc = STORE_NAME, ind = Index 21, a = Just (Arg 5)},
InstructionWInd {opc = JUMP_ABSOLUTE, ind = Index 22, a = Just (Arg 8)},

InstructionWInd {opc = LOAD_NAME, ind = Index 23, a = Just (Arg 4)},
InstructionWInd {opc = LOAD_NAME, ind = Index 24, a = Just (Arg 1)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 25, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 26, a = Nothing},
InstructionWInd {opc = LOAD_CONST, ind = Index 27, a = Just (Arg 2)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 28, a = Nothing}]
'''









'''
    x = 10
    y = 5
    z = 0
    if x < y: 
        x = x + 10
        print(x) 
    elif x > y: 
        x = x - 10 
        print(x) 
    else: 
        print("HELLO")


0,10
10,20
20,24
24,34
34,40


[InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},
InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},
InstructionWInd {opc = STORE_NAME, ind = Index 3, a = Just (Arg 1)},
InstructionWInd {opc = LOAD_CONST, ind = Index 4, a = Just (Arg 2)},
InstructionWInd {opc = STORE_NAME, ind = Index 5, a = Just (Arg 2)},
InstructionWInd {opc = LOAD_NAME, ind = Index 6, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_NAME, ind = Index 7, a = Just (Arg 1)},
InstructionWInd {opc = COMPARE_OP, ind = Index 8, a = Just (Arg 0)},
InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 9, a = Just (Arg 20)},

InstructionWInd {opc = LOAD_NAME, ind = Index 10, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_CONST, ind = Index 11, a = Just (Arg 0)},
InstructionWInd {opc = BINARY_ADD, ind = Index 12, a = Nothing},
InstructionWInd {opc = STORE_NAME, ind = Index 13, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_NAME, ind = Index 14, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_NAME, ind = Index 15, a = Just (Arg 0)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 16, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 17, a = Nothing},
InstructionWInd {opc = LOAD_CONST, ind = Index 18, a = Just (Arg 3)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 19, a = Nothing},

InstructionWInd {opc = LOAD_NAME, ind = Index 20, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_NAME, ind = Index 21, a = Just (Arg 1)},
InstructionWInd {opc = COMPARE_OP, ind = Index 22, a = Just (Arg 4)},
InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 23, a = Just (Arg 34)},

InstructionWInd {opc = LOAD_NAME, ind = Index 24, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_CONST, ind = Index 25, a = Just (Arg 0)},
InstructionWInd {opc = BINARY_SUBTRACT, ind = Index 26, a = Nothing},
InstructionWInd {opc = STORE_NAME, ind = Index 27, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_NAME, ind = Index 28, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_NAME, ind = Index 29, a = Just (Arg 0)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 30, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 31, a = Nothing},
InstructionWInd {opc = LOAD_CONST, ind = Index 32, a = Just (Arg 3)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 33, a = Nothing},

InstructionWInd {opc = LOAD_NAME, ind = Index 34, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_CONST, ind = Index 35, a = Just (Arg 4)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 36, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 37, a = Nothing},
InstructionWInd {opc = LOAD_CONST, ind = Index 38, a = Just (Arg 3)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 39, a = Nothing}]

'''










'''
[InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},
InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},
InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},
InstructionWInd {opc = STORE_NAME, ind = Index 3, a = Just (Arg 1)},

InstructionWInd {opc = LOAD_NAME, ind = Index 4, a = Just (Arg 2)},
InstructionWInd {opc = LOAD_CONST, ind = Index 5, a = Just (Arg 2)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 6, a = Just (Arg 1)},
InstructionWInd {opc = GET_ITER, ind = Index 7, a = Just (Arg 1)},
InstructionWInd {opc = FOR_ITER, ind = Index 8, a = Just (Arg 6)},

InstructionWInd {opc = STORE_NAME, ind = Index 9, a = Just (Arg 3)},
InstructionWInd {opc = LOAD_NAME, ind = Index 10, a = Just (Arg 4)},
InstructionWInd {opc = LOAD_NAME, ind = Index 11, a = Just (Arg 0)},
InstructionWInd {opc = CALL_FUNCTION, ind = Index 12, a = Just (Arg 1)},
InstructionWInd {opc = POP_TOP, ind = Index 13, a = Nothing},
InstructionWInd {opc = JUMP_ABSOLUTE, ind = Index 14, a = Just (Arg 8)},

InstructionWInd {opc = LOAD_CONST, ind = Index 15, a = Just (Arg 3)},
InstructionWInd {opc = RETURN_VALUE, ind = Index 16, a = Nothing}]
'''

