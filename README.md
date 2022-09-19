# Zbuild

zbuild is a modern makefile-like build automation tool.

# Example

Zbuild uses a yaml file to specify all rules. It will look for for the nearest `zbuild.yaml` file in the current or parent directories.

Assuming the following directory structure for your project:

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

This zbuild manifest can build anything in the `build` directory

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
    # These are the items that your rule outputs.
    #
    # They may contain files, simply represented by strings, or
    # make-like dependency files, represented by `deps_file: <file>`.
    # If these dependency files exist at the start of the program, all dependencies within it will be considered.
    out: [$(output)]

    # Dependencies
    # These are the items that your rule requires.
    #
    # These may contain files (simply strings), dependency files (`deps_file: <file>`), other rules (`rule: <rule-name>`) or
    # static items `static: <non-rule item>`.
    #
    # Static dependencies are only built if they don't exist. if they are out of date, as long as they exist, they won't be rebuilt
    deps: [$(input)]

    # Execution
    #
    # Specifies an array of commands to execute to build this
    # rule. Each command is an array of arguments.
    exec:
      - [gcc, $(input), -c, -o, $(output)]

  compile_out:
    out: [$(build_dir)/my_proj.out]
    deps: [$(build_dir)/a.o, $(build_dir)/b.o]

    exec:
      - [gcc, $(build_dir)/a.o, $(build_dir)/b.o, -o, $(build_dir)/my_proj.out]
```
