from setuptools import setup

setup(name='imageblobber',
      version='0.1',
      description='Blob images.',
      author='fizzoo',
      scripts=['bin/imageblobber.py'],
      install_requires=['opencv-python', 'numpy', 'tqdm'])
