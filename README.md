# Cinder

Cinder is a build system designed specifically for the Obsidian programming language. Written in Haskell, Cinder provides a simple, efficient way to automate the building and compilation of Obsidian projects. Its syntax follows a familiar Makefile-style format, making it easy to define tasks, dependencies, and execution flows.

# Features

- Customizable Build Logic: Define complex build steps and workflows using a simple and declarative syntax.
- Task Dependencies: Specify dependencies between tasks to ensure proper build order.
- Fast Execution: Cinder is designed for speed, making your build process efficient and reliable.
- Seamless Integration: Easily integrate with other tools and compilers, making Cinder a flexible option for larger projects.

# Installation

To install Cinder, you’ll need to have GHC (Glasgow Haskell Compiler) installed on your machine. You can download GHC from [here](https://www.haskell.org/ghcup/install/)

After installing GHC, follow these steps:
1. Clone the Cinder repository:
```bash
git clone https://github.com/obsidian-language/cinder.git
```
2. Navigate to the project directory:
```bash
cd cinder
```
3. Build and install Cinder:
```bash
cabal install
```
4. Add the Cinder binary to your system path (optional but recommended):
```bash
export PATH=$PATH:/path/to/cinder/bin
```

# Usage
To use Cinder for your Obsidian project, create a cinderfile in the root directory of your project. The cinderfile defines the tasks and dependencies for the build process.

## Basic Example:
```
helloWorld: main.ob
    obsidian @$ -o $!
```

## Running Cinder:

To start a build, simply run:
```
cinder
```
This will execute the build task along with its dependencies.