from __future__ import print_function
import bencode
import socket
import pprint
import sys


ADDRESS = '127.0.0.1'
PORT = int(sys.argv[1])

def build_eval(data):
    return {"op": "eval", "code": data.strip()}

def repl():
    sock = socket.socket()
    sock.connect((ADDRESS, PORT))
    sock.settimeout(0.5)

    while True:
        data = raw_input("> ")
        if data.strip():
            if data.startswith('\\'):
                sock.send(bencode.bencode(eval(data[1:])))
            else:
                sock.send(bencode.bencode(build_eval(data)))

        try:
            incoming = sock.recv(4096)
            if incoming:
                print("Message:")
                print(repr(incoming))
                pprint.pprint(bencode.bdecode(incoming))
                print()
        except socket.timeout:
            pass


if __name__ == '__main__':
    repl()
