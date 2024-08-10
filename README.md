# Zbuild

zbuild is a modern makefile-like build automation tool.

# Guide

Zbuild uses a yaml file to specify all rules. It will look for for the nearest `zbuild.yaml` file in the current or parent directories.

See the [Guide](./GUIDE.md) for a proper introduction to zbuild.

# Examples

See the `examples/` directory for examples.

In particular the `examples/simple_c/zbuild.yaml` is a fully documented example that helps you understand how zbuild works.

# Watching

Zbuild can "watch" your build, re-running rules whenever files are updated. This can be activated with the `--watch` flag.
See `zbuild --help` for more details.

# Installation

You may download the latest release from the [releases page](https://github.com/Zenithsiz/zbuild/releases).

On arch-based systems, you can install it using the `PKGBUILD` included in this repo.

You may also compile it yourself, with a nightly rust compiled (>= rustc 1.68.0-nightly (9c07efe84 2022-12-16), may work with older) and install it by:

1. Clone the repo

   `git clone https://github.com/Zenithsiz/zbuild`

2. Build + install

   `cargo install --path zbuild`

# License

This project is dual-licensed under either of

- [Apache license Version 2.0](LICENSE-APACHE)
- [MIT license](LICENSE-MIT)

# Contribution

Feel free to open any issues with any bugs, opinions or suggestions you have
on the project.
