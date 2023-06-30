# Require by String

## Summary

We need to add intuitive alias functionality and fix existing issues with `require` statements to facilitate the grouping together of related Luau files into libraries and allow for future package managers to be developed and integrated easily.

## Motivation

The Roblox engine does not currently support require-by-string. One motivation for this RFC is to consolidate require syntax and functionality between the Roblox engine and the broader Luau language itself.

Luau itself currently supports a basic require-by-string syntax that allows for requiring Luau modules by relative or absolute path. Unfortunately, the current implementation has a few issues.

### Relative paths

Currently, relative paths are always evaluated relative to the current working directory that the Luau CLI is running from. This leads to unexpected behavior when requiring modules from "incorrect" working directories.

Suppose the module `math.luau` is located in `/Users/JohnDoe/LuauModules/Math` and contains the following:

```lua=
-- Beginning of /Users/JohnDoe/LuauModules/Math/math.luau
local sqrt = require("../MathHelperFunctions/sqrt")
```

If we then launched the Luau CLI from the directory `/Users/JohnDoe/Projects/MyCalculator` and required `math.luau` as follows:

```lua
> local math = require("/Users/JohnDoe/LuauModules/Math/math")
```

This would cause the following:
- The current implementation of require would successfully find `math.luau`, as its absolute path was given.
- Then, it would execute the contents of `math.luau` from the context of the current working directory `/Users/JohnDoe/Projects/MyCalculator`.
- When attempting to require `sqrt.luau` from `math.luau`, instead of looking in the directory `/Users/JohnDoe/LuauModules/MathHelperFunctions`, the relative path will be evaluated in relation to the current working directory.
- This will look for `sqrt.luau` in `/Users/JohnDoe/Projects/MathHelperFunctions`, which is a directory that may not even exist.

This behavior is [problematic](https://github.com/Roblox/luau/issues/959), and puts an unnecessary emphasis on the directory from which the Luau CLI is running. A better solution is to evaluate relative paths in relation to the file that is requiring them.


### Package management

While package management itself is outside of the scope of this RFC, we want to make it possible for a package management solution to be developed in a way that integrates well with our require syntax.

To require a Luau module under the current implementation, we must require it either by relative or absolute path:

```lua
-- Requiring Roact by absolute path
local Roact = require("C:/LuauModules/Roact-v1.4.2")
```

If we wanted to change the location of this package on disk, we would have to modify all files that require the old path above, which essentially boils down to manual package management. This approach is tedious and prone to mistakes.

To solve this, we introduce aliases in this RFC, which would allow for future package management software to maintain package paths and versions by directly modifying alias maps.

This would also create simple and readable require statements for developers.

```lua
-- Suppose "@src" is an alias for the same directory as "../../../../../"

-- Instead of this:
local result = require("../../../../../HelperModules/graphing")

-- We could have this:
local result = require("@src/HelperModules/graphing")
```

## Design

### String syntax

Our require syntax should:

- Allow a library written in Luau to be imported into the Roblox engine and "just work".
- Support compile-time file resolution _where possible_ for type checking.
- Be consistent across platforms and both inside and outside of Roblox.
- Provide support for binding unambiguous aliases to relative and absolute paths.
- Attempt to remain consistent with the existing Luau path syntax.

#### Relative paths

Modules can be required relative to the requiring file's location in the filesystem.

If we are trying to require a module called `MyModule.luau` in `C:/MyLibrary`:
```lua
-- From C:/MyLibrary/SomeModule.luau
local MyModule = require("MyModule")

-- Same as above, identical behavior
local MyModule = require("./MyModule")

-- From C:/MyLibrary/SubDirectory/SubModule.luau
local MyModule = require("../MyModule")
 
-- From C:/MyOtherLibrary/MainModule.luau
local MyModule = require("../MyLibrary/MyModule")
```

Relative paths are chosen as the default path type, meaning that if a given path does not begin with a reserved prefix such as `/` or `@`, then it is considered relative to the requiring file's location. This is chosen for convenience, as libraries may include various internal dependencies that are easier to reference using relative paths.

Relative paths can also begin with `./` or `../`, which denote the directory of the requiring file and its parent directory, respectively. When executing Luau code directly from a CLI (e.g. in the Luau CLI), this is relative to the current working directory.

#### Absolute paths

Modules can also be required by their absolute path by prefixing the root of the filesystem (e.g. `C:/` or `/`).

Requiring a file called `MyModule.luau` in `<ROOT>/MyLibrary`:
```lua
-- Relative to root directory "/" (Linux/Unix)
local MyModule = require("/MyLibrary/MyModule")

-- Relative to root directory "C:/" (Windows)
local MyModule = require("C:/MyLibrary/MyModule")

-- These also work, "\" is internally replaced with "/"
-- (In a string literal, we must use the escape sequence "\\")
local MyModule = require("\\MyLibrary\\MyModule")
local MyModule = require("C:\\MyLibrary\\MyModule")
```

Generally, absolute paths should not be used in regular code: if the location of a globally installed package changes, it can be a nuisance to find and change all require statements that reference the old absolute path.

Instead, absolute paths should appear only in the alias map. Then, only the alias map will need to be modified if a package's location is changed on disk, and in the future, package management software could automatically manage the alias map rather than needing to reach into and modify individual Luau files.

#### Aliases

Aliases can be used to bind an absolute or relative path to a convenient name that can be required directly. In Luau files, aliases will always be prefixed with `@` to unambiguously distinguish them from directory names.

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

Note that the alias map itself does not contain any `@` prefixes, which differs from some other languages. For example, when configuring paths in `tsconfig.json` for TypeScript projects, the `@` prefix is optional. Instead, the alias name is matched exactly in require statements. To reduce ambiguity in Luau files, we will always require `@` to be prepended to the alias name that appears in the alias map.

##### Versioning

Aliases are simple bindings and aren't concerned with versioning. The intention is to allow package management software to leverage aliases by automatically adding and updating the alias map to reflect a package's dependencies.

##### Library root alias

In the past, it has been proposed to use the blank alias "`@`" to represent the root directory of a file's encapsulating library. 
- However, the concept of a "Luau library" and its root directory is not yet rigorously defined in Luau, in terms of folder/file structure. 
- In the future, we may add a `package.json` file or something similar that marks the root directory of a library, but this is outside of the scope of this RFC, which primarily focuses on improving require-by-string.
- For the time being, this functionality will remain unimplemented for this reason. The blank alias "`@`" will remain reserved for now, meaning it cannot be overriden.

Of course, users can still use the alias map to explicitly define this behavior with a named alias:

```json
"aliases": {
    "src": "../"
}
```

##### Limitations of aliases

- Aliases cannot reference other aliases.
- Alias names cannot contain the directory separators `/` and `\`.
- All aliases must be prefixed with `@` when used in a require statement to avoid ambiguity.
- Aliases can only occur at the beginning of a path.

##### Configuring alias maps: .luaurc

As part of this proposal, alias maps will be configured in `.luaurc`, which follows a JSON-like syntax.

Proposed structure of an alias map:
```json
{
    "aliases": {
        "alias1": "/path/of/alias1",
        "alias2": "/path/of/alias2"
    }
}
```

Missing aliases in `.luaurc` are inherited from the alias maps of any parent directories, and fields can be overriden.

Additionally, if an alias is bound to a relative path, the path will evaluated relative to the `.luaurc` file in which the alias was defined.

#### Path resolution

If we find files with the same name but different extensions, then we will attempt to require a file with the following extensions (in this order):
1. `.luau`
2. `.lua`
3. All other file extensions are invalid.

If the string resolves to a directory instead of a file, then we will attempt to require a file in that directory with the following name (in this order):
1. `init.luau`
2. `init.lua`

#### DataModel as VFS

In the Roblox engine, the DataModel will act as a virtual file system. At the root of this VFS is the DataModel itself. This will allow packages from Luau to be imported and exported freely without needing to consider the platform they are being used on.

All paths used in the Roblox engine must refer to a location in the DataModel. They cannot be used to access files on disk.

```lua
-- MyModule location: game:GetService("ReplicatedStorage").MyModule

-- Require by absolute path
local MyModule = require("/ReplicatedStorage/MyModule")

-- Require by relative path
-- (From game:GetService("ReplicatedStorage").MyOtherModule)
local MyModule = require("MyModule")
```

##### Considerations

Within the Roblox engine, we will have to handle replication and waiting for modules to be loaded into the DataModel before requiring them. However, this is outside of the scope of this RFC and will be discussed internally.

#### Platform compatibility

For compatibility across platforms, we will automatically map `/` onto `\`.

## Drawbacks

### Backwards compatibility

Luau libraries are already not compatible with existing Lua libraries. This is because Lua favors the `.` based require syntax instead and relies on the `LUA_PATH` environment variable to search for modules, whereas Luau currently supports a basic require-by-string syntax.

- Libraries are fully compatible with the Roblox engine, as require-by-string is currently unimplemented.
- Luau currently implements relative paths in relation to the current working directory. 
- We propose changing this behavior, and breaking backwards compatibility on this front.
- With the current implementation, requiring a library that itself contains relative-path require statements [can become a mess](https://github.com/Roblox/luau/issues/959) if the Luau VM is not launched from the "correct" working directory.
- We propose the following change: relative paths passed to require statements will be evaluated in relation to the requiring file's location, not in relation to the current working directory.
    - Caveat: if the relative-path require is called directly in the Luau CLI, this will remain relative to the current working directory, as there is no sense of a "requiring file" here.
- If this causes issues, we can later introduce a default alias for the current working directory (e.g. `@CWD`).

## Alternatives

### Prohibiting absolute paths in require

Again, using aliases to refer to absolute paths is encouraged over directly passing absolute paths to require. To promote this, we might consider prohibiting the passing of absolute paths to require statements altogether.

There are two main reasons for *not* doing this.

#### Fast prototyping and testing

First, aliases certainly make code simple and readable, but we still want to allow developers to prototype and test their code quickly. Requiring developers to set up/update their alias map each time they want to refer to a package by its absolute path isn't ideal.

If absolute paths are prohibited in require statements altogether, developers might lean toward using relative paths, which often take more effort to understand:
```lua
-- This is long, but the directory structure is clear
local result = require("Users/JohnDoe/Luau/LuauModules/HelperModules/graphing")

-- This is shorter, but it is unclear which directory each "../" refers to
local result = require("../../../../../HelperModules/graphing")
```

#### Backwards compatibility

Second, backwards compatibility. Luau currently uses `fopen`/`_wfopen` to locate required modules, which both support absolute paths. Any existing code that relies on absolute paths being passed to require statements will break if this behavior is changed.

### Different ways of defining aliases

#### Defining aliases directly in the requiring file

Rather than defining an alias map in an external configuration file, we could alternatively define aliases directly in the files that require them. For example, this could manifest itself through an extension of the `--!` comment syntax or introduce new syntax like `--@<ALIAS> = @<PATH>`.
```lua
--@"Roact" = @"C:/LuauModules/Roact-v1.4.2"
local Roact = require("@Roact")

-- Same as this:
local Roact = require("@C:/LuauModules/Roact-v1.4.2")
```

Some potential issues with this approach:
- Could lead to Luau file headers becoming cluttered.
- Would probably lead to substantial copy-and-pasting between modules in the same library, as they would likely need to share certain aliases.
- Using configuration files for alias maps allows modules to share aliases while still providing the flexibility to override if needed. This approach does not support inheritance and overriding in an obvious way.
- Removes the layer of abstraction that is provided by external alias maps. This might also blur the scope of package managers. The software would need to directly modify lines of code in `.luau` files, rather than modifying configuration files.

#### Defining configuration files in a non-JSON format

Alias maps could alternatively be defined in specially named `.luau` files themselves, or at least adhering to Luau syntax. This approach is somewhat unappealing, however, as it would require package management software to parse Luau syntax. JSON syntax is well-understood and well-supported, which would likely faciliate the development of package management software.