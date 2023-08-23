# Usage guide

> **Important**:
>
> See the [Unimplemented features](#Unimplemented-features) section for possible missing features described here
>
> Also see the [Limitations](#Limitations) section for details on why a certain feature may not be working properly yet.

## Core concepts

The 2 main core concepts used in zbuild are:

- Rules
- Items

Rules are objects which describe an action that can be taken to generate files.

Items can be files, rules, or even just some condition[^1]. Generally they are split into output items and dependency items.

Output items are those that a rule creates. These _cannot_ be other rules, as you cannot "create" a rule. Generally they are files created by the execution of the rule.

Dependency items are more general, in that they can be any kind of dependency: A file, a rule execution or conditions.

See [Rule Outputs](#rule-outputs) and [Rule Dependencies](#rule-dependencies) for more details on items.

See the [Rule](#rule) section about details on rules.

[^1]: Currently unimplemented

## Rules

Rules are the core concept of using zbuild, they are your way to explain to zbuild which actions do what.

By specifying an output and dependencies, whenever zbuild is asked for a target to build, it can analyze all rules and determine a way to generate the file required.

Rules have the following sections:

- `alias`: Aliases
- `out`: Outputs
- `deps`: Dependencies
- `exec`: Execution

Each one is optional, and may be skipped if irrelevant to the rule.

### Rule Aliases

(Also see: [Aliases](#aliases) section)

Rule aliases are scoped so that they are only resolved inside the body of the rule they're defined in. They _may_ shadow global aliases and will take priority.

### Rule Outputs

(Also see: [Dependency Files](#dependency-files) and sections)

Output items are those that you guarantee to `zbuild` that will exist after the execution of the rule.

They may be either regular files, or dependency files.

If they are dependency files, they will be considered dependencies,
_if they exist_ when the rule dependencies are checked.

You **must** guarantee that, if the rule is executed successfully all files actually exist.

The modification date of the rule output is the oldest file among all of the outputs.

Output items may include patterns, specified as `^(<pattern-name>)` inside any string. When checking if a rule can create an output, the pattern will be tested to see if any string substituted into the pattern outputs the item. If so, this string is given the string value for the whole rule.

These patterns may be used as dependencies, but only if the output items can resolve them.

Patterns may also include operators. Specified as `^(<pattern-name>::<op1>::<op2>)`. The following operators are currently supported:

1. `non_empty`
   Ensures the pattern cannot match an empty string

You may specify them as an array of the following:

1. Regular file

   Regular file are specified simply by a string with their path

2. Dependency files

   Dependency files are specified with `deps_file: <file>` where `<file>` is the path of the file

### Rule Dependencies

(Also see: [Dependency Files](#dependency-files) )

Dependency items are those that you require to exist before the rule can be executed.

These may be either:

- (Static or Optional) Regular files
- (Static or Optional) Dependency files
- Rules

Static items are those that only need to exist in order to be considered up to date. They are useful for depending on, e.g. directories.

Regular files are just that, regular files, their modification date will be used to test if the output needs to be rebuilt.

Dependency files are regular files which are also checked for make dependencies after being considered up to date.

Optional file are regular files that do not result in an error if they do not exist.

Rule dependencies simply state that a rule must be run before them.
Even if two rules required the same rule in their dependencies, it will only be run once (per pattern). Rules are always considered out of date and will always be executed.

You may specify them as an array of the following:

1. Static item

   Static items are specified with `static: <item>` where `<item>` is either a regular file or a dependency item

2. Optional file

   Optional files are specified with `opt: <item>` where `<item>` is either a regular file or a dependency item

3. Regular file

   Regular file are specified simply by a string with their path

4. Dependency files

   Dependency files are specified with `deps_file: <file>` where `<file>` is the path of the file

5. Rules

   Rules are specified with `{ rule: <rule-name>, pats: { <pat1>: <value1>, <pat2>: <value2 >} }`.

   Where `<rule-name>` is the rule name, and `patX` is a pattern for the rule, with `valueX` as it's value.

   For rules without patterns, you may simply use `rule: <rule-name>`.

### Rule execution

The execution of a rule consists in running a series of specified commands.

It should be noted that all commands are _not_ run in a shell. If you require a shell you may spawn it yourself with a command.

Each command is executed sequentially given it's order's rule.
However, it may be run parallel to other rules (or even the same rule with a different pattern).

You may also define a working directory for a command to be executed in. You may only do this at the granularly of the rule, not per command however.

A rule is executed whenever it becomes a target. This happens, for e.g. when you specify a rule name on the command line, or when zbuild determines the output files are out of date relative to the dependencies.

When specifying a rule as a dependency, it's output files will be used for checking if out of date. If the rule has no output files, the instant it finished executing will be used.

You may specify the execution as either:

1. Short form

   In the short form you simply specify all commands to run in an array.

   You may not specify any options (such as the working directory) using this form

   ```
   exec:
     - [bash, ...]
     - [cp, ...]
     - ...
   ```

2. Full form

   The full form allow you to fully specify everything, but you must put the arguments of each command within an inner `args` key.

   ```
   exec:
      - cwd: "..."
        args: [bash, ...]
      - [cp, ...]
      - ...
   ```

## Aliases

You may define aliases (either global or rule-scoped) which give a name to a value.

They are typically used to reduce the repetition of a rule, or abstract a certain value (such as an output directory) behind a variable, so it may be more easily changed.

You may instantiate an alias using `$(<alias>)` where `<alias>` is your alias' name.

Aliases may also include patterns. When matching rule patterns, all aliases are expanded and only patterns remain, if they contain patterns.

Aliases may also include operators. Specified as `$(<alias>::<op1>::<op2>)`. The following operators are currently supported:

1. `dir_name`
   Resolves the alias into a path, then returns the parent path (without a trailing `/`).

## Dependency files

zbuild has limited support for make dependency files, typically generated by `gcc -MF <file>` or other tools.

zbuild can read these files both as output dependencies for a rule or as input dependencies.

For most use cases you will likely have them as output dependencies, but having them as input dependencies has a huge advantage if the dependencies of a file (file-a) depend on another (file-b) and you don't want to hard-code theses dependencies onto the zbuild manifest.

You can instead create a program that reads file-b, outputs a dependency file, then specify that file as a dependency of file-a, and zbuild will ensure that all dependencies are generated.

## Limitations

Dependency files may currently only specify a single dependency, which must be:

1. If the rule has any outputs, any of the outputs
2. If the rule has no dependencies, the name of the rule

## Unimplemented features

Currently conditions are not implemented in any way, neither checking or depending on then

Alias operators cannot be used if the value has any patterns when used in the rule output section. This would complicate pattern matching severely, so it has not been implemented yet.

Glob dependencies currently don't exist.
