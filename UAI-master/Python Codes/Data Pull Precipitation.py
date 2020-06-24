from setuptools import setup, find_packages

import ecmwfapi


setup(
    name="ecmwf-api-client",
    version=ecmwfapi.__version__,
    description=ecmwfapi.__doc__,
    author="ECMWF",
    author_email="software.support@ecmwf.int",
    url="https://software.ecmwf.int/stash/projects/PRDEL/repos/ecmwf-api-client/browse",

    # entry_points={
    #     "console_scripts": [
    #         "mars = XXX:main",
    #     ],
    # },

    packages=find_packages(),
    zip_safe=False,
)