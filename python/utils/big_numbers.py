#! /usr/local/bin/python3.6
"""
Comparison between two big-digit integers (or floats) without using Decimal.
"""

import sys
import traceback
from typing import Union

class CompareBigDigits:
    def __init__(self, l: Union[int, float, str], r: Union[int, float, str]):
        if not isinstance(l, (int, float, str)) or not isinstance(r, (int, float, str)):
            raise ValueError("l and r must be integers, floats, or strings")
        
        # Convert to string to handle all cases
        self.l_str = self.__normalize_number(str(l))
        self.r_str = self.__normalize_number(str(r))

        # Extract sign
        self.sign_l = -1 if self.l_str.startswith('-') else 1
        self.sign_r = -1 if self.r_str.startswith('-') else 1

        # Remove negative sign for digit comparison
        self.l_str = self.l_str.lstrip('-')
        self.r_str = self.r_str.lstrip('-')

        # Split integer and decimal parts
        self.l_int, self.l_dec = self.__split_number(self.l_str)
        self.r_int, self.r_dec = self.__split_number(self.r_str)

    def __normalize_number(self, num_str: str) -> str:
        """ Convert numbers into a clean string format without scientific notation. """
        if 'e' in num_str or 'E' in num_str:
            num_str = format(float(num_str), 'f')  # Convert scientific notation to full number
        return num_str

    def __split_number(self, num_str: str):
        """ Split a number string into integer and decimal parts. """
        if '.' in num_str:
            int_part, dec_part = num_str.split('.')
        else:
            int_part, dec_part = num_str, ''
        return int_part, dec_part

    def exec(self) -> int:
        """ Execution """
        try:
            return self.__compare()
        except Exception as e:
            raise

    def __compare(self) -> int:
        """ Compare numbers digit-by-digit. """
        try:
            # ✅ First, compare by sign
            if self.sign_l > self.sign_r:
                return 1
            elif self.sign_l < self.sign_r:
                return -1

            # ✅ Compare integer part length (longer = greater)
            if len(self.l_int) > len(self.r_int):
                return self.sign_l
            elif len(self.l_int) < len(self.r_int):
                return -self.sign_l

            # ✅ Compare integer digits one by one
            for i in range(len(self.l_int)):
                if self.l_int[i] > self.r_int[i]:
                    return self.sign_l
                elif self.l_int[i] < self.r_int[i]:
                    return -self.sign_l

            # ✅ Compare decimal digits one by one
            max_length = max(len(self.l_dec), len(self.r_dec))
            l_dec = self.l_dec.ljust(max_length, '0')  # Pad with zeroes for equal length
            r_dec = self.r_dec.ljust(max_length, '0')

            for i in range(max_length):
                if l_dec[i] > r_dec[i]:
                    return self.sign_l
                elif l_dec[i] < r_dec[i]:
                    return -self.sign_l

            return 0  # ✅ Numbers are equal
        except Exception as e:
            raise

    def __display(self, s: int) -> None:
        """ Display """
        try:
            print("\n[LEFT]  =", self.l_str)
            print("[RIGHT] =", self.r_str)
            sign = "=" if s == 0 else (">" if s > 0 else "<")
            print("[LEFT] {} [RIGHT]".format(sign))
        except Exception as e:
            raise


if __name__ == '__main__':
    try:
        obj = CompareBigDigits("1234567890.12345678901234567890", 1234567890.12345678901234567890)
        result = obj.exec()
        print(result)
    except Exception as e:
        traceback.print_exc()
        sys.exit(1)
