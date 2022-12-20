import sys
import marshal
import dis


def view_src_and_pyc(path, filepath):
    """Read and display a content of the Python`s bytecode and source to bytecode in a pyc-file."""

    with open(filepath, 'r') as f:
        filecontent = f.read()
        c = compile(filecontent, '', "exec")
        print("\n\n\n")
        codes = [c for c in c.co_code]
        for i in range(int(len(codes)/2)):
            print("opcode:", codes[2*i], "oparg", codes[(2*i)+1])
            pass

    f.close()

    print("\n-----------\n")

    with open(path, 'rb') as f:
        f.seek(16)
        content = f.read()
        print(content)
        print("\n\n\n")

        f.seek(16)
        dis.dis(marshal.load(f))

    f.close()


def view_pyc(path):
    """Read and display a content of the Python`s bytecode in a pyc-file."""

    with open(path, 'rb') as f:
        f.seek(16)
        content = f.read()
        print(content)
        print("\n\n\n")

        f.seek(16)
        dis.dis(marshal.load(f))

    f.close()


view_src_and_pyc(sys.argv[1], sys.argv[2])
# view_pyc(sys.argv[1])
