#!/usr/bin/env python3

import sys

startOfPacketWidth = 4
startOfMessageWidth = 14

for line in sys.stdin:
    line = line.strip()
    for i in range(0,len(line)-startOfPacketWidth):
        candidate = line[i:i+startOfPacketWidth]
        #print(f'{candidate}: {len(set(candidate))}')
        if (len(set(candidate)) == startOfPacketWidth):
            print(f'Start of first packet: {i+startOfPacketWidth}')
            break
    for i in range(0,len(line)-startOfMessageWidth):
        candidate = line[i:i+startOfMessageWidth]
        #print(f'{candidate}: {len(set(candidate))}')
        if (len(set(candidate)) == startOfMessageWidth):
            print(f'Start of first message: {i+startOfMessageWidth}')
            break

