import numpy as np

def activation(net: float, a: float) -> int:
    if net > a: return 1
    elif net < -a: return -1
    else: return 0

def training():
    pass