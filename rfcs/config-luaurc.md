# Configure analysis via .luaurc

## Summary

Introduces a way to configure type checker and linter using JSON-like .luaurc files

## Motivation

While Luau analysis tools try to provide sensible defaults, it's difficult to establish the rules that work for all code.
For example, some packages may decide that unused variables aren't interesting; other packages may decide that all files should be using strict typechecking mode.

While it's possible to configure some aspects of analysis behavior using --! comments, it can be cumbersome to replicate this in all files.

## Design

To solve this problem, we are going to introduce support for `.luaurc` files for users of command-line Luau tools.
For a given .lua file, Luau will search for .luaurc files starting from the folder that the .lua file is in; all files in the ancestry chain will be parsed and their configuration
applied. When multiple files are used, the file closer to the .lua file overrides the settings.

.luaurc is a JSON file that can also contain comments and trailing commas. The file can have the following keys:

- `"mode"`: type checking mode, can be one of "nocheck", "nonstrict", "strict"
- `"lint"`: lints to enable; points to an array of string literals that correspond to the names of linting rules (see https://luau-lang.org/lint), or `"*"` that means "all rules"
- `"nolint"`: lints to disable; points to an array of string literals or `"*"`
- `"lintErrors"`: a boolean that controls whether lint issues are reported as errors or warnings (off by default)
- `"typeErrors"`: a boolean that controls whether type issues are reported as errors or warnings (on by default)
- `"globals"`: extra global values; points to an array of strings where each string names a global that the type checker and linter must assume is valid and of type `any`

Example of a valid .luaurc file:

```json5
{
	"mode": "nonstrict",
	"lint": "*",
	"nolint": ["LocalUnused"],
	"lintErrors": true,
	"globals": ["expect"] // TestEZ
}
```

In addition to the JSON file, for consistency this proposal suggests allowing to *enable* lints on a per-file basis with `--!lint` hot comment, just like you can disable them with `--!nolint`.

Note that in absence of a configuration file, we will use default settings: mode will be set to nonstrict, a set of lint warnings is going to be enabled by default (this proposal doesn't detail that set - that will be subject to a different proposal), type checking issues are going to be treated as errors, lint issues are going to be treated as warnings.

## Design -- compatibility

Today we support .robloxrc files; this proposal will keep parsing legacy specification of configuration for compatibility:

- Top-level `"language"` key can refer to an object that has `"mode"` key that also defines language mode
- Top-level `"lint"` key can refer to an object that has lint names as keys and `"disabled"`/`"enabled"`/`"fatal"` as values.

These keys are only going to be supported for compatibility and only when the file name is .robloxrc (which is only going to be parsed by internal Roblox command line tools but this proposal mentions it for completeness).

## Drawbacks

The introduction of configuration files means that it's now impossible to type check or lint sources in isolation, which complicates the code setup.

File-based JSON configuration may or may not map cleanly to environments that don't support files, such as Roblox Studio.

Using JSON5 instead of vanilla JSON limits the interoperability.

## Alternatives

It's possible to consider forcing users to specify the source settings via `--!` comments exclusively. This is problematic as it may require excessive amounts of annotation though, which this proposal aims to simplify.

The format of the configuration file does not have to be JSON; for example, it can be a valid Luau source file which is the approach luacheck takes. This makes it more difficult to repurpose the .luaurc file to use third-party processing tools though, e.g. a package manager would need to learn how to parse Luau syntax to store configuration in .luaurc.
