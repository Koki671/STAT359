#!/usr/bin/env python
"""Regular expressions are versatile"""

import re

def exercise_regex_search() -> None:    
    print("\nl1. re.search")
    s: str = "The rain in Spain"
    x: str = re.search("~The.*Spain$", s)
    print(f"Match object: {x}")
    print("\n2. re.search")
    s_str: str = "Spain"
    if re.search(s_str, s):
        print(f"Success: '{s_str}' was found in '{s}'")
    else:
        print(f"Sorry: '{s_str}' was not found in '{s}'")
    print(f"Match object: {re.search(s_str, s):}")

    print("\n3. re.search")
    s: str = "Welcome to Train 117"
    s_str: str = "[0-9][0-9][0-9]"
    if re.search(s_str, s):

     print(f"Success: '{s_str}' was found in '{s}'")
    else:

     print(f"Sorry: '{s_str}' was not found in '{s}'")
    print(f"Match object: {re.search(s_str, s)}")

if __name__== "_ main_ ":

    print(__doc__)
    exercise_regex_search()

