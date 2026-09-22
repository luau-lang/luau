# Luau golden tests

The `tools.golden` package runs source-level end-to-end tests through both `luau-analyze` and
`luau`. Discovery is structural: every `.luau` file under `tests/golden` is a
test. No directive is needed to register one — a test is defined by the files on
disk, and its expectations come from adjacent output files and/or an optional
`--!golden` directive.

## Quick start

Create `tests/golden/example.luau`. Every `.luau` file is a test; add a directive
or an output file so its outcome is checked:

```luau
--!golden ok

local answer: number = 42
assert(answer == 42)
```

Then, from the Luau source root, run:

```sh
python3 -m tools.golden
```

The runner executes every test under both flag configurations and prints the
resolved `luau` and `luau-analyze` paths at startup.

To experiment with fast flags across the whole suite, pass the same value used
by the Luau CLIs:

```sh
python3 -m tools.golden \
  --fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
```

`--fflags` can be repeated. Each value is forwarded to all three commands in
the order supplied.

To start with exact snapshots instead of a directive, create the `.luau` file
and populate all of its expectations in one step:

```sh
python3 -m tools.golden --update=all --config=all example
```

## Layout and test IDs

Any `.luau` file that is not inside an `init.luau` folder is its own single-file
test:

```text
tests/golden/
└── types/
    └── generic.luau       # test ID: types/generic
```

A folder that contains `init.luau` is a single multifile test entered through
that `init.luau`. Everything nested anywhere beneath it — including files in
subfolders — is part of that one test. Those files are helpers; their contents,
including any stray directive, are ignored:

```text
tests/golden/
└── packages/
    └── cycle/             # test ID: packages/cycle
        ├── init.luau      # entry point and directives
        ├── first.luau     # helper
        └── nested/
            └── second.luau  # helper (part of packages/cycle)
```

`init.luau` cannot sit directly in `tests/golden`; multifile tests must have a
named subdirectory so they also have a usable test ID.

The outermost `init.luau` wins: a deeper `init.luau` is a helper of the enclosing
unit, not the root of its own. Because helpers only exist inside an `init.luau`
folder, a stray `.luau` outside one is a test in its own right and must resolve an
outcome (see below), or discovery fails.

IDs always use forward slashes and are relative to `tests/golden`; single-file
IDs omit `.luau`, while multifile IDs name the directory. Select one or more
test IDs or directory IDs as positional arguments. A directory ID selects every
test transitively beneath it:

```sh
python3 -m tools.golden types/generic packages/cycle
python3 -m tools.golden analysis/tables
python3 -m tools.golden analysis
```

Selection is not glob-based. Overlapping selections do not run a test more than
once. IDs that match neither a test nor a directory containing tests, duplicate
test IDs, orphan output files, misplaced directives, and an empty suite are
errors.

## Directives

Directives are optional and attach expectations to a test. When present, they
must be the first nonblank lines in the entry point (an entry point is a
single-file test or an `init.luau`). The shorthand form supplies one expected
status for every configuration:

```luau
--!golden ok
```

A test with no directive at all is valid when each flag configuration has at
least one adjacent exact-output file, which then provides the assertion:

```luau
local answer: number = 42
assert(answer == 42)
```

The full grammar is a comma-separated list of `key: value` assignments:

```text
--!golden status: ok, flags-off.status: fail
--!golden strict.output: /common output/
--!golden flags-on.nonstrict.output: /new diagnostic/
```

Tests can also force fast flags to a fixed value in every configuration. The
value uses the same comma-separated format as the Luau CLIs' `--fflags` option:

```luau
--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
```

Put `fflags=` on its own directive line because commas are part of its value.
The runner passes this override after the configuration's `--fflags=true` or
`--fflags=false`, so named flags keep the requested value in both configurations.
Bare flag names enable those flags, and explicit `=true` and `=false` values
enable or disable them. Whole-set `true` and `false` entries are also accepted,
matching the CLI behavior. Only one `fflags=` directive is allowed per test.

Supported keys are:

- `status` and `CONFIG.status`
- `[CONFIG.]COMMAND.output`

The configurations are `flags-on` and `flags-off`; commands are `strict`,
`nonstrict`, and `runtime`. Status is either `ok` or `fail`.
A configuration-specific status takes precedence over the global status. Each
configuration must resolve to a status or have at least one exact-output file.

Status `ok` requires all three commands to exit 0. Status `fail` requires at
least one command to exit 1; the others may exit 0 or 1. Status describes the
combined analyzer/interpreter result and cannot be command-specific in the MVP.
Only exit 1 satisfies `fail`; a signal, timeout, or any other abnormal exit is a
harness failure and always fails the test regardless of its expected status.
When status is omitted for a configuration, command exit codes are not
validated for that configuration.

Output values are Python regular expressions delimited by `/`. Commas inside
the delimiters do not split assignments. `\/` represents a literal slash.
Python inline flags are supported, and every applicable repeated pattern must
match using `re.search`:

```luau
--!golden status: fail
--!golden strict.output: /type.*mismatch/
--!golden nonstrict.output: /(?im)^.*expected number.*$/
--!golden runtime.output: /https:\/\/example\.test\/result/
```

Malformed directives, invalid regexes, unknown keys/configurations, duplicate
statuses at the same scope, and configurations with neither status nor exact
output fail discovery before any test process starts. A line beginning with
`--!golden` must use that exact marker followed by whitespace; spellings such
as `--!golden:` or `--!golden-status` are rejected rather than treated as Luau
source.

## Configurations

Each selected test is run independently under this fixed matrix:

| Configuration | `strict` | `nonstrict` | `runtime` |
| --- | --- | --- | --- |
| `flags-on` | `luau-analyze --mode=strict --solver=new --fflags=true ENTRY` | `luau-analyze --mode=nonstrict --solver=new --fflags=true ENTRY` | `luau --fflags=true ENTRY` |
| `flags-off` | `luau-analyze --mode=strict --solver=new --fflags=false ENTRY` | `luau-analyze --mode=nonstrict --solver=new --fflags=false ENTRY` | `luau --fflags=false ENTRY` |

`--fflags=true` and `--fflags=false` affect the complete CLI fast-flag set,
not only the type solver. Runner-level `--fflags` values are applied after the
base value. A test's `--!golden fflags=...` value is applied last, so its local
requirement takes precedence over suite-wide experiments. Both analyzer modes
use the new solver, and runtime uses the default interpreter. To add a command
or flag configuration, add a `Command` or `Configuration` entry in
`tools/golden/models.py`.
Execution, directive validation, exact-output filenames, CLI selection, and
help text derive from those entries; update this documentation and the unit
tests at the same time.

The suite-level `tests/golden/.luaurc` disables the `CommentDirective` lint so
`--!golden` metadata does not pollute analyzer output expectations.

## Exact output

Exact-output files are flat and adjacent to their entry point:

```text
example.luau
example.flags-on.strict.output
example.flags-off.nonstrict.output
example.flags-off.runtime.output
```

For a multifile test, the prefix is `init`, so expectations sit beside
`init.luau` as `init.flags-on.strict.output`, and so on. A present file must
match exactly; a missing file means that command's output is ignored. An empty
file requires empty output. Files must be UTF-8.

An exact-output file also makes `ok`/`fail` optional for its flag
configuration. Exact comparison then provides the assertion, while process
exit codes are ignored unless a status directive is also present. Regex
directives do not make status optional.

The `.output` suffix is reserved for exact expectations throughout the golden
test tree. Every such file must follow the naming scheme above and refer to a
real entry point; misspelled configurations, commands, and orphan files fail
discovery instead of being silently ignored.

An exact file and a regex directive cannot govern the same configuration and
command. Global regex directives govern that command's output in both
configurations and therefore conflict with either configuration's exact file.

CRLF and lone CR line endings are normalized to LF in both captured and exact
output before comparison. Exact mismatches are reported as unified diffs;
regex mismatches include the captured output.

## Comparing and updating

Comparison mode always runs both configurations:

```sh
python3 -m tools.golden
python3 -m tools.golden types/generic packages/cycle
```

Targeted update requires at least one ID. It creates one combined output file
for each selected command, including empty files. It can bootstrap a freshly
added directive-free test when the selected configurations cover every
configuration that does not already have a status or exact output:

```sh
python3 -m tools.golden --update=strict --config=flags-on types/generic
python3 -m tools.golden --update=nonstrict,strict \
  --config=flags-on,flags-off types/generic
python3 -m tools.golden --update=nonstrict --config=all packages/cycle
python3 -m tools.golden --update=all --config=all runtime/assertions
```

Suite-wide update accepts no IDs and rewrites only exact files that already
exist:

```sh
python3 -m tools.golden --update-all=strict,runtime --config=flags-off
python3 -m tools.golden --update-all=strict --update-all=runtime \
  --config=flags-on --config=flags-off
python3 -m tools.golden --update-all=all --config=all
```

`--update`, `--update-all`, and `--config` accept one matrix value, a
comma-separated list, repeated options, or `all`. `all` cannot be combined with
named values. `--update` and `--update-all` remain mutually exclusive.

## Inspecting captured output

`--dump` writes what each run actually produced next to its expected file, so a
mismatch can be opened or diffed directly instead of read out of the console:

```sh
python3 -m tools.golden --dump types/generic
```

For every command in every selected configuration it writes
`<entry>.<config>.<command>.output.tmp` beside the entry point — the expected
`.output` name plus a `.tmp` suffix, so the pair sorts together:

```text
example.flags-on.strict.output       # expected (committed)
example.flags-on.strict.output.tmp   # last captured run (gitignored)
```

The flag is independent of comparison and update modes; it only records output
and never affects pass/fail. Commands that hit a harness failure (timeout,
spawn error, and so on) captured no usable output and are skipped. The `.tmp`
files are throwaway: they are overwritten each run, ignored by discovery
(the `.output` scheme is unaffected), and matched by `Client/Luau/.gitignore`
so they cannot be committed.

`--clean` sweeps them away again. It deletes every `*.output.tmp` file under
the test root and exits without running any test, so it needs neither the CLIs
nor a valid corpus:

```sh
python3 -m tools.golden --clean
```

It removes only the throwaway files; committed `.output` expectations never
carry the `.tmp` suffix and are left alone. `--clean` takes no test IDs and
cannot be combined with `--dump`, `--update`, or `--update-all`.

Both update modes still run all three commands and validate status when present.
Regex-governed outputs are validated but never rewritten. Unselected
expectations are still compared. Configuration runs execute concurrently, but
validation and writes happen in lexical order, so output and update behavior are
deterministic; writes are atomic, ordinary assertion/status failures do not stop
later tests, and a harness failure suppresses all writes for its
test/configuration.

## Finding the executables

Build-system targets pass exact executable paths. For manual runs, override
either or both paths explicitly:

```sh
python3 -m tools.golden --luau ./build/debug/luau \
  --luau-analyze ./build/debug/luau-analyze

LUAU=./build/debug/luau \
LUAU_ANALYZE=./build/debug/luau-analyze \
python3 -m tools.golden
```

Missing executables are resolved in order:

1. beside an explicitly resolved counterpart;
2. a colocated pair in the current working directory;
3. a colocated pair in the Luau source root;
4. a unique pair in common Make/CMake build directories;
5. the platform `PATH`.

Normal names and `.exe` names are recognized. Candidates must be executable.
Discovery prefers colocated pairs, never builds tools, and rejects ambiguous
build results with the candidate paths so they can be supplied explicitly.

## Process and data rules

Commands use argument arrays without a shell, execute with `tests/golden` as
their working directory, and receive forward-slash relative entry paths. The
default timeout is 10 seconds and can be changed with `--timeout SECONDS`.
`--test-root PATH` overrides the corpus location. Configuration runs execute
concurrently; `--jobs N` sets the pool size, where `0` (the default) auto-sizes
to the machine and `1` forces serial execution.

Exit 0 and 1 are test outcomes. Spawn failures, timeouts, signals, all other
exit codes, and invalid UTF-8 output are harness failures. Output from all three
commands is decoded strictly as UTF-8. Each command's stderr is redirected to
stdout before launch, preserving their combined pipe order.

## Troubleshooting and MVP limits

- “No status resolves” means that configuration has neither an applicable
  status directive nor an exact-output file. A brand-new `.luau` with no
  directive and no output files trips this until you add one or the other.
- “Misplaced directive” means code or another nonblank line appeared before a
  golden directive in an entry point.
- “Orphan exact-output file” means its adjacent `.luau` entry is absent or is a
  helper inside an `init.luau` folder rather than an entry point.
- “Root-level init.luau is not supported” means a multifile entry point was
  placed directly in `tests/golden`; move it into a named subdirectory.
- “Regex and exact expectations” means one command has two incompatible sources
  of truth; remove either the regex or exact file.
- Executable ambiguity is resolved with `--luau` and `--luau-analyze`.
- A timeout can be investigated with a larger `--timeout`; do not encode a
  timeout as expected failure.

The MVP has no old-solver, codegen, or alternate-interpreter configurations;
no command-specific status; no per-test CLI arguments other than fast flags;
no glob selection; and no automatic directive rewriting. Regex expectations
are always maintained by hand.
