#!/usr/bin/env python

# {{{ Imports

import math

# }}}

# {{{ Arithmetic

print(1+1)
print(math.sqrt(2))

# }}}

# <<< Functions


def apply_sqrt(*args):
    """Apply sqrt to list"""
    return [math.sqrt(x) for x in args]


print(apply_sqrt(1, 2, 3, 4, 5))

# >>>
