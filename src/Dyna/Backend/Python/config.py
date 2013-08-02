import os
from IPython.external.path import path

dotdynadir = path('~/.dyna').expand()
if not dotdynadir.exists():
    dotdynadir.mkdir()

dynahome = path(os.getenv('DYNAHOME', '.'))
