import os
from path import path

dotdynadir = path('~/.dyna').expand()
if not dotdynadir.exists():
    dotdynadir.mkdir()

dynahome = path(os.getenv('DYNAHOME', '.'))
