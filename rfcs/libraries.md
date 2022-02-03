# Libraries

## Summary

We need a way to group together related code and provide easy ways to require them.

## Motivation

- Package management
- Third party libraries
- Simpler requires

## Design

### String Syntax

Our require syntax should:

- Work the same way in the Roblox engine and Luau language
- Be resolvable at compile-time

#### Absolute

Modules can be required by their absolute path from the root of the filesystem.

Requiring a file called `MyModule.luau` in `C:/MyLibrary`:
```lua=
local MyModule = require("C:/MyLibrary/MyModule")
```

#### Relative

Modules can be required relative to the requiring files location in the filesystem.

If we are trying to access a file called `MyModule.luau` in `C:/MyLibrary`:
```lua=
-- From C:/MyLibrary/SubDirectory/SubModule.luau
local MyModule = require("../MyModule")
 
-- From C:/MyOtherLibrary/MainModule.luau
local MyModule = require("../MyLibrary/MyModule")
```

All relative paths will start with either `./`  or `../`  which denote the directory of the script or file, and parent directory respectively.

#### Aliases

Aliases can be used to bind an absolute or relative path to a convenient name that can be required directly. They are always prefixed with `$` to make it obvious they are not the same as a regular path.

While the format is yet to be decided it will be a map that exists as part of a library and is defined in a format such as JSON.

```json=
{
    "Roact": "C:/LuauModules/Roact-v1.4.2"
}
```

Based on that map you would be able to require Roact directly:

```lua=
local Roact = require("$Roact")
```

Or even a sub-module:

```lua=
local createElement = require("$Roact/createElement")
```

##### Versioning

Aliases are simple bindings and aren't concerned with versioning. The intention is for a package manager to leverage aliases by automatically adding and updating the alias map to reflect a packages dependencies.

##### Root Alias

All libraries have a special alias for referring to their root. This alias cannot be overriden and is simply defined as `$`.

Requiring any file in the library can be done relative to the root:
```lua=
local ModuleAtLibraryRoot = require("$/ModuleAtLibraryRoot")
```

##### Inheritance

Libraries can be embedded in other libraries. In this case aliases are inherited from the parent.

##### Limitations

- Aliases cannot reference other aliases
- Aliases cannot contain certain characters such as `/` or `$` (full list TBD)
- All aliases must be prefixed with $ when used in a require statement to avoid ambiguity
- Aliases can only occur at the beginning of a path
- Multiple aliases are not supported

#### Directories

If the string resolves to a directory rather than a file then we will attempt to require a specific file in that directory with the following name:
1. `init.lua`
2. `init.luau`

#### DataModel as VFS

In the Roblox engine, the DataModel will act as a VFS with the DataModel itself at the root. This will allow packages from Luau to be imported and exported freely without needing to consider the platform they are being used on.

#### Backwards Compatibility

Possibly not. TBD

### Defining a Library

TBD

## Drawbacks

TBD

## Alternatives

TBD