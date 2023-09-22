from setuptools import setup, find_packages

setup(
    name="lispy",
    version="0.1",
    packages=find_packages(),
    entry_points={"console_scripts": ["lispy = lispy:main"]},
    # Metadata
    # could be any number of things, including install_requires
    install_requires=[
        "click",
        "prompt_toolkit",  # etc...
    ],
)

# Generate .gitignore for python
