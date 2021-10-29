# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
from enum import Enum
import sys

class Color(Enum):
    DEFAULT = 0
    RED = 1
    GREEN = 2
    BLUE = 3
    YELLOW = 4
    WHITE = 5

def colored_on(color:Color, message:str):
    from colorama import Fore, Style
    color_mappings = {
        Color.DEFAULT: (Fore.WHITE, Style.NORMAL),
        Color.RED: (Fore.RED, Style.NORMAL),
        Color.GREEN: (Fore.GREEN, Style.NORMAL),
        Color.BLUE: (Fore.BLUE, Style.BRIGHT),
        Color.YELLOW: (Fore.YELLOW, Style.NORMAL),
        Color.WHITE: (Fore.WHITE, Style.BRIGHT)
    }
    fore, style = color_mappings[color]
    return fore + style + message + Style.RESET_ALL

def colored_off(color:Color, message:str):
    return message

try:
    if sys.stdout.isatty():
        import colorama
        colorama.init()
        colored = colored_on
    else:
        colored = colored_off
except:
    colored = colored_off
