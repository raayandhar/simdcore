from abc import ABC, abstractmethod


# 8 2 3 4 5 6 7 1
# 

# 0b0000 0000

# length of array must always be less than or equal to 2
def enc(i:str) -> list[int]:
    pass


# mov v1, v1 [1 2 3 4 5 6 7 8]

assert enc("1 2 3 4 5 6 7 8") == [0x00]


def dec(i:list[int]) -> str:
    pass

"""
total bit encodings: 64

right rotate permutation (8 possibilites)
0b0000 0000 - no rotation
0b0000 0001 - 1 rotation
0b0000 0010 - 2 rotation
0b0000 0011 - 3 rotation
0b0000 0100 - 4 rotation
0b0000 0101 - 5 rotation
0b0000 0110 - 6 rotation
0b0000 0111 - 7 rotation

pair wise swap (56 possibilities)
0b0000 0000 - swap 0,1
0b0000 0001 - swap 0,2
0b0000 0001 - swap 0,3
0b0000 0001 - swap 0,4
0b0000 0001 - swap 0,5
0b0000 0001 - swap 0,6
0b0000 0001 - swap 0,7
0b0000 0001 - swap 0,8

0b0000 0000 - swap 1,0
0b0000 0001 - swap 1,2
0b0000 0001 - swap 1,3
0b0000 0001 - swap 1,4
0b0000 0001 - swap 1,5
0b0000 0001 - swap 1,6
0b0000 0001 - swap 1,7
0b0000 0001 - swap 1,8

0b0000 0000 - swap 2,0
0b0000 0001 - swap 2,1
0b0000 0001 - swap 2,3
0b0000 0001 - swap 2,4
0b0000 0001 - swap 2,5
0b0000 0001 - swap 2,6
0b0000 0001 - swap 2,7
0b0000 0001 - swap 2,8

0b0000 0000 - swap 3,0
0b0000 0001 - swap 3,1
0b0000 0001 - swap 3,2
0b0000 0001 - swap 3,4
0b0000 0001 - swap 3,5
0b0000 0001 - swap 3,6
0b0000 0001 - swap 3,7
0b0000 0001 - swap 3,8

0b0000 0000 - swap 4,0
0b0000 0001 - swap 4,1
0b0000 0001 - swap 4,2
0b0000 0001 - swap 4,3
0b0000 0001 - swap 4,5
0b0000 0001 - swap 4,6
0b0000 0001 - swap 4,7
0b0000 0001 - swap 4,8

0b0000 0000 - swap 5,0
0b0000 0001 - swap 5,1
0b0000 0001 - swap 5,2
0b0000 0001 - swap 5,3
0b0000 0001 - swap 5,4
0b0000 0001 - swap 5,6
0b0000 0001 - swap 5,7
0b0000 0001 - swap 5,8

0b0000 0000 - swap 6,0
0b0000 0001 - swap 6,1
0b0000 0001 - swap 6,2
0b0000 0001 - swap 6,3
0b0000 0001 - swap 6,4
0b0000 0001 - swap 6,5
0b0000 0001 - swap 6,7
0b0000 0001 - swap 6,8

0b0000 0000 - swap 7,0
0b0000 0001 - swap 7,1
0b0000 0001 - swap 7,2
0b0000 0001 - swap 7,3
0b0000 0001 - swap 7,4
0b0000 0001 - swap 7,5
0b0000 0001 - swap 7,6
0b0000 0001 - swap 7,8
"""

class Shuffle(ABC):
    pass

@dataclass
class ClockwiseRotate(Shuffle):
    code: string
    def decode()

@dataclass
class Swap(Shuffle):
    code: string




