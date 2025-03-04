from setuptools import setup, find_packages
from typing import List

HYPHEN_E_DOT = '-e .'
def read_requirements(file_path: str) -> List[str]:
    requirements = []
    with open('requirements.txt') as f:
        requirements = f.readlines()

        if HYPHEN_E_DOT in requirements:
            requirements.remove(HYPHEN_E_DOT)
    return requirements


setup(
    name="my_ml_project",
    version="1.0.0",
    author="Ahmed",
    author_email="ahmed.lapip51@gmail.com",
    description="A Machine Learning project for prediction",
    packages=find_packages(),
    install_requires=read_requirements(),
    python_requires=">=3.8",
)