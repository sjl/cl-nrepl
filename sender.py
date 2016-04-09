# coding=utf-8
from __future__ import print_function
import bencode
import socket
import sys

from pygments import highlight
from pygments.lexers import PythonLexer
from pygments.formatters import TerminalFormatter
from pprint import pformat


ADDRESS = 'localhost'
PORT = int(sys.argv[1])

def build_eval(data):
    return {"op": "eval", "code": data.strip()}

def build_doc(data):
    return {"op": "documentation", "symbol": data.strip()}

def pprint(obj):
    # ...........………………_„-,-~''~''':::'':::':::::''::::''~
    # ………._,-'':::::::::::::::::::::::::::::::::::::::::::''-„
    # ………..,-':::::::::::::::::::::::::::::::::::::::::::::::
    # ………,-'::::::::::::::::::::::::::„:„„-~-~--'~-'~--~-~--~-
    # ……..,'::::::::::,~'': : : : : : : : : : : : : : : : '-|
    # ……..|::::::::,-': : : : : : : : - -~''''¯¯''-„: : : : :\
    # ……..|:::::::: : : : : : : : : _„„--~'''''~-„: : : : '|
    # ……..'|:::::::,': : : : : : :_„„-: : : : : : : : ~--„_: |'
    # ………|:::::: : : „--~~'''~~''''''''-„…_..„~''''''''''''¯|
    # ………|:::::,':_„„-|: : :_„---~: : ''¯¯''''|: ~---„_:   ||
    # ……..,~-,_/'': : : |: _ o__): : |: :: : : : _o__): \..|
    # ……../,'-,: : : : : ''-,_______,-'': : : : ''-„_____|
    # ……..\: : : : : : : : : : : : : : :„: : : : :-,: : :\
    # ………',:': : : : : : : : : : : : :,-'__: : : :_', ;: ,'
    # ……….'-,-': : : : : :___„-: : :'': : ¯''~~'': ': : ~--|'
    # ………….|: ,: : : : : : : : : : : : : : : : : : : :: :
    # ………….'|: \: : : : : : : : -,„_„„-~~--~--„_: :: : : |
    # …………..|: \: : : : : : : : : : : :-------~: : : : : |
    # …………..|: :''-,: : : : : : : : : : : : : : : : : :
    # …………..',: : :''-, : : : : : : : : : : : :  : :: ,'
    # ……………| : : : : : : : : :_ : : : : : : : : : : ,-'
    # ……………|: : : : : : : : : : '''~----------~''
    # …………._|: : : : : : : : : : : : : : : : : : :
    # ……….„-''. '-,_: : : : : : : : : : : : : : : : : ,'
    # ……,-''. . . . . '''~-„_: : : : : : : : : : : : :,-'''-„
    #              █▀█░█▀█░█▀█░█░█▄░█░▀█▀░
    #              █▀▀░█▀▀░█▀▄░█░█▀██░░█░░
    #              ▀░░░▀░░░▀░▀░▀░▀░░▀░░▀░░
    print(highlight(pformat(obj), PythonLexer(), TerminalFormatter()))

def parse_fucked_bencode_data(data):
    # im so sorry about this
    while data:
        for i in xrange(1, len(data)+1):
            try:
                yield bencode.bdecode(data[:i])
                break
            except:
                continue
        data = data[i:]

def repl():
    sock = socket.socket()
    sock.connect((ADDRESS, PORT))
    sock.settimeout(0.5)

    while True:
        data = raw_input("> ")
        if data.strip():
            if data == 'quit':
                return

            if data.startswith('\\d'):
                sock.send(bencode.bencode(build_doc(data[2:])))
            elif data.startswith('\\'):
                sock.send(bencode.bencode(eval(data[1:])))
            else:
                sock.send(bencode.bencode(build_eval(data)))

        try:
            incoming = sock.recv(1024*1024) # one megabyte ought to be enough for anybody
            if incoming:
                print("Message(s):")
                print(incoming)
                try:
                    pprint(bencode.bdecode(incoming))
                except bencode.BTL.BTFailure:
                    for m in parse_fucked_bencode_data(incoming):
                        pprint(m)
                print()
        except socket.timeout:
            pass


if __name__ == '__main__':
    repl()
