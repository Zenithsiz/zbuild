# Zbuild

zbuild is a modern makefile-like build automation tool.

# Guide

See the [Guide](./GUIDE.md) for a proper introduction to zbuild.

# Example

Zbuild uses a yaml file to specify all rules. It will look for for the nearest `zbuild.yaml` file in the current or parent directories.

If you have a project in C, with the following directory structure, the zbuild specified can build your project.

```
my_proj/
	build/
		a.o
		b.o
		my_proj.out
	src/
		a.c
		b.c
```

`zbuild.yaml`:

```yaml
# Global aliases
# These are aliases that may be used in the whole manifest.
# You can reference them by using `$(<name>)` at any point.
alias:
  build_dir: build
  src_dir: src

# Default targets
# These are the targets (i.e. files / rules) that will be built
# when no command line arguments are given.
default:
  - $(build_dir)/my_proj.out

# Rules
# zbuild will examine these to figure out how to build any target
rules:
  # Each rule has a unique name, used only for it's identification
  compile_o:
    # Rule aliases
    # Rules may contain scoped aliases, specific to them.
    alias:
      # `^(...)` specifies a pattern to match, every other instance of it will be replaced in the rest of the rule.
      # Patterns are simple string replacements currently.
      # You may also only have a single pattern per rule currently.
      output: $(build_dir)/^(name).o
      input: $(src_dir)/^(name).c

    # Output items
    # These are the items that your rule outputs.considered.
    out: [$(output)]

    # Dependencies
    # These are the items that your rule requires.
    deps: [$(input)]

    # Execution
    #
    # Specifies an array of commands to execute to build this rule. Each command is an array of arguments. It will not be passed to a shell, but instead be executed as-is
    exec:
      - [gcc, $(input), -c, -o, $(output)]

  compile_out:
    out: [$(build_dir)/my_proj.out]
    deps: [$(build_dir)/a.o, $(build_dir)/b.o]

    exec:
      - [gcc, $(build_dir)/a.o, $(build_dir)/b.o, -o, $(build_dir)/my_proj.out]
```

Then simply run `zbuild` and it will build (or rebuild, if out of date) the final binary.

# Installation

You may download the latest release from the [releases page](https://github.com/Zenithsiz/zbuild/releases).

You may also compile it yourself, with a nightly rust compiled (>= rustc 1.66.0-nightly (432abd86f 2022-09-20), may work with older) and install it by:

1. Clone the repo

   `git clone https://github.com/Zenithsiz/zbuild`

2. Build + install

   `cargo install --path zbuild`
