# -*- coding: utf-8 -*-

def encode (value):
    pure = []

    while value >= 128:
        rest = value & 0b01111111 
        pure.append (rest)
        value = value >> 7

    pure.append (value)
    pure [0] += 1 << 7
    pure.reverse ()

    buffer = [chr (code) for code in pure]
    return "".join (buffer)

def decode (value):
    result = 0

    buffer = [ord (byte) for byte in value]
    for byte in buffer:
        result = result << 7
        result += byte

    result = result - (1 << 7)
    return result