# Libraries

## Summary

We need a way to group together related code into libraries and provide easy ways to require them.

## Motivation

- Package management
- Third party libraries
- Simpler requires

## Design

### String Syntax

Our require syntax should:

- Allow a library written in Luau to be imported into the Roblox engine and 'just work'.
- Support compile-time file resolution _where possible_ for type checking.
- Be consistent across platforms and both inside and outside of Roblox.

#### Relative

Modules can be required relative to the requiring file's location in the filesystem.

If we are trying to require a module called `MyModule.luau` in `C:/MyLibrary`:
```lua
-- From C:/MyLibrary/SubDirectory/SubModule.luau
local MyModule = require("../MyModule")
 
-- From C:/MyOtherLibrary/MainModule.luau
local MyModule = require("../MyLibrary/MyModule")
```

Relative paths are the default path type, meaning that if a given path does not begin with a reserved prefix such as `/` or `@`, then it is considered relative to the requiring file's location. Relative paths can begin with `../`, which denotes the parent directory of the requiring script, or the parent of the current working directory for the CLI.

#### Absolute

Modules can be required by their absolute path by prefixing the root of the filesystem (e.g. `C:/` or `/`).

Requiring a file called `MyModule.luau` in `<ROOT>/MyLibrary`:
```lua
-- Relative to root directory "/" (Linux/Unix)
local MyModule = require("/MyLibrary/MyModule")

-- Relative to root directory "C:/" (Windows)
local MyModule = require("C:/MyLibrary/MyModule")

-- This also works, "\" is internally replaced with "/"
local MyModule = require("C:\MyLibrary\MyModule")
```

Generally, absolute paths should not be used in regular code and should only appear in the alias map. Their purpose is to allow libraries to be stored in a global location (such as if packages are installed globally) without breaking if the libraries' locations are changed on disk.

#### Aliases

Aliases can be used to bind an absolute or relative path to a convenient name that can be required directly. They are always prefixed with `@` to unambiguously distinguish them from directory names. Note that the alias map itself does not contain any `@` prefixes; these are required by default when requiring by alias in Luau scripts.

Each library has its own alias map which will be stored in a `luauconfig.json` file. Since libraries can be embedded inside of one another, any aliases which are not overriden will be inherited from parent libraries.

```json
"aliases": {
    "Roact": "C:/LuauModules/Roact-v1.4.2"
}
```

Based on the alias map above, you would be able to require Roact directly:

```lua
local Roact = require("@Roact")
```

Or even a sub-module:

```lua
local createElement = require("@Roact/createElement")
```

##### Versioning

Aliases are simple bindings and aren't concerned with versioning. The intention is for a package manager to leverage aliases by automatically adding and updating the alias map to reflect a package's dependencies.

##### Root Alias

The blank alias "`@`" cannot be overriden and will remain reserved for now. It has been proposed in the past to use "`@`" to represent the root directory of a script's encapsulating library, but this will remain unimplemented for the time being. Users can use the alias map to explicitly define this behavior, if desired:

```json
"aliases": {
    "MML": "C:/LuauModules/MyMathLibrary"
}
```

##### Limitations

- Aliases cannot reference other aliases.
- Alias names cannot contain certain characters such as `/`, `@`, or `.` (full list TBD).
- All aliases must be prefixed with `@` when used in a require statement to avoid ambiguity.
- Aliases can only occur at the beginning of a path.
- Multiple aliases are not supported.

#### File Resolution

If the string resolves to a directory rather than a file, then we will attempt to require a file in that directory with the following name (in this order):
1. `init.luau`
2. `init.lua`

If multiple files match the given path, we will attempt to require a file with the following extensions (in this order):
1. `.luau`
2. `.lua`
3. All other file extensions are invalid.

#### DataModel as VFS

In the Roblox engine, the DataModel will act as a virtual file system. At the root of this VFS is the DataModel itself. This will allow packages from Luau to be imported and exported freely without needing to consider the platform they are being used on.

All paths used in the Roblox engine must refer to a location in the DataModel, they cannot be used to access files on disk.

```lua
-- TODO: EXAMPLE NEEDED
```

#### Platforms

For compatability across platforms, we will automatically map `/` onto `\`.

#### Backwards Compatibility

Luau libraries are already not compatible with existing Lua libraries. This is because Lua favors the `.` based require syntax instead and relies on the `LUA_PATH` environment variable to search for modules, whereas Luau currently implements a basic require-by-string syntax.

- Libraries are fully compatible with the Roblox engine, as require-by-string is currently unsupported.
- Luau currently implements relative paths in relation to the current working directory. We propose changing this behavior, and breaking backwards compatibility on this front.
  - With the current implementation, requiring a library that itself contains relative-path require statements can become a mess if the Lua VM is not launched from the "correct" working directory.
  - We propose taking a "script-first" approach: relative paths passed to require statements will be considered in relation to the requiring script's location, not the current working directory.
  - If this causes issues, we can introduce a default alias for the current working directory (e.g. `@CWD`).

### luauconfig.json

As part of this proposal, we will introduce a new `luauconfig.json` file. Initially, this file will only include library configuration data but in the future could be expanded to contain more.

The proposed structure for this file is:

```json
{
    "aliases": {
        "alias": "./path/of/alias"
    }
}
```

Missing fields in `luauconfig.json` are inherited (and can be overriden) from any existing parent/grandparent libraries.

### Defining a Library

Any directory containing a `luauconfig.json` file is considered a library.

Since we don't support `.json` files in the DataModel we will introduce a new `Library` instance which contains the alias map and other properties.

## Drawbacks

TBD

## Alternatives

TBD
