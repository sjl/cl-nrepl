from __future__ import print_function
import bencode
import socket


ADDRESS = '127.0.0.1'
PORT = 8675

def repl():
    sock = socket.socket()
    sock.connect((ADDRESS, PORT))
    sock.settimeout(0.5)

    while True:
        data = raw_input("> ")
        if data.strip():
            sock.send(bencode.bencode(eval(data)))

        try:
            incoming = sock.recv(4096)
            if incoming:
                print(bencode.bdecode(incoming))
        except socket.timeout:
            pass


if __name__ == '__main__':
    repl()
