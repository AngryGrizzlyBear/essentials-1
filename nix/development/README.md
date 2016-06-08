# Development

This directory contains everything needed to get started developing in my nix build. A key feature is the `env` directory
that contains a number of environments used to set up nix shells for isolated development.

## The Nix Shells

Inside `env` are the nix shells I use to develop. These contain specific packages for that language. Common packages across languages are moved into the default.nix file in the `development` directory to save typing.

These shells are designed to get you up and running *quickly* and are not designed to be a perfect fit for any project. For project-based nix shells, you should build them yourself. You're welcome to use the files in `env` as a base.

## Using the Nix Shells

Simply `cd` in `env` and then `cd` into the language folder of your choice. Type `nix-shell --pure` and it will drop you into a configured nix shell you can then use for development. Each language has its own nuances - for example in `python` suggests you setup a `virtualenv` for each project.
