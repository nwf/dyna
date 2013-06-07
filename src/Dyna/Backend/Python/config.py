import os
from path import path

dotdynadir = path('~/.dyna').expand()
if not dotdynadir.exists():
    dotdynadir.mkdir()

dynahome = os.getenv('DYNAHOME', '.')
