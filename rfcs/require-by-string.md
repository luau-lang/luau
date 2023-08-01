# Require by String

## Summary

We need to add intuitive alias functionality and fix existing issues with `require` statements to facilitate the grouping together of related Luau files into libraries and allow for future package managers to be developed and integrated easily.

## Motivation

The Roblox engine does not currently support require-by-string. One motivation for this RFC is to consolidate require syntax and functionality between the Roblox engine and the broader Luau language itself.

Luau itself currently supports a basic require-by-string syntax that allows for requiring Luau modules by relative or absolute path. Unfortunately, the current implementation has a few issues.

### Relative paths

Currently, relative paths are always evaluated relative to the current working directory that the Luau CLI is running from. This leads to unexpected behavior when requiring modules from "incorrect" working directories.

Suppose the module `math.luau` is located in `/Users/JohnDoe/LuauModules/Math` and contains the following:

```lua
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

To solve this, we introduce path and alias configuration in this RFC, which would allow for future package management software to maintain package paths and versions by directly modifying configuration files.

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

For compatibility across platforms, we will automatically map `/` onto `\`.

#### Path resolution

If we find files with the same name but different extensions, then we will attempt to require a file with the following extensions (in this order):
1. `.luau`
2. `.lua`
3. All other file extensions are invalid.

If the string resolves to a directory instead of a file, then we will attempt to require a file in that directory with the following name (in this order):
1. `init.luau`
2. `init.lua`

#### Relative paths

Modules can be required relative to the requiring file's location in the filesystem (note, this is different from the current implementation, which evaluates all relative paths in relation to the current working directory).

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

Relative paths can begin with `./` or `../`, which denote the directory of the requiring file and its parent directory, respectively. When a relative path does begin with one of these prefixes, it will only be resolved relative to the requiring file. If these prefixes are not provided, path resolution will fallback to checking the paths in the `paths` configuration variable, as described later.

When a require statement is executed directly in a REPL input prompt (not in a file), relative paths will be evaluated in relation to the pseudo-file `stdin`, located in the current working directory. If the code being executed is not tied to a file (e.g. using `loadstring`), executing any require statements in this code will result in an error.

#### Absolute paths

Absolute paths will no longer be supported in `require` statements, as they are unportable. The only way to require by absolute path will be through a explicitly defined paths or aliases defined in configuration files, as described later.

#### Paths

Similar to [paths in TypeScript](https://www.typescriptlang.org/tsconfig#paths), we will introduce a `paths` array that can be configured in `.luaurc` files. Whenever a path is passed to `require` and does not begin with `./` or `../`, the path will first be resolved relative to the requiring file. If this fails, we will attempt to resolve paths relative to each path in the `paths` array.

The `paths` array can contain absolute paths, and relative paths are resolved relative to `.luaurc` file in which they appear.

##### Example Definition

With the given `paths` definition (`.luaurc` file located in `/Users/johndoe/Projects/MyProject/src`):
```json
"paths": [
    "../dependencies",
    "/Users/johndoe/MyLuauLibraries",
    "/Users/johndoe/MyOtherLuauLibraries",
]
```

If `/Users/johndoe/Projects/MyProject/src/init.luau` contained the following code:
```lua
local graphing = require("graphing")
```
We would search the following directories, in order:
- `/Users/johndoe/Projects/MyProject/src`
- `/Users/johndoe/Projects/MyProject/dependencies`
- `/Users/johndoe/MyLuauLibraries`
- `/Users/johndoe/MyOtherLuauLibraries`

#### Aliases

Aliases can be used to bind an absolute or relative path to a convenient, case-sensitive name that can be required directly.

```json
"aliases": {
    "Roact": "C:/LuauModules/Roact-v1.4.2"
}
```

Based on the alias map above, you would be able to require Roact directly:

```lua
local Roact = require("Roact")
```

Or even a sub-module:

```lua
local createElement = require("Roact/createElement")
```

Aliases are overrides. Whenever the first component of a path exactly matches a pre-defined alias, it will be replaced before the path is resolved to a file.

##### Versioning

Aliases are simple bindings and aren't concerned with versioning. The intention is to allow package management software to leverage aliases by automatically adding and updating the alias map to reflect a package's dependencies. For manual versioning, a user could always "version" their aliases: `@MyModule-v1`, `@MyModule-v2`, etc.

##### Library root alias

In the past, it has been proposed to define alias (e.g. "`@`") to represent the root directory of a file's encapsulating library. 
- However, the concept of a "Luau library" and its root directory is not yet rigorously defined in Luau, in terms of folder/file structure. 
- In the future, we may add a `package.json` file or something similar that marks the root directory of a library, but this is outside of the scope of this RFC, which primarily focuses on improving require-by-string.
- For the time being, this functionality will remain unimplemented for this reason. The alias "`@`" will remain reserved for now, meaning it cannot be overridden.

Of course, users can still use the alias map to explicitly define this behavior with a named alias:

```json
"aliases": {
    "src": "../"
}
```

##### Current limitations of aliases

- Aliases cannot reference other aliases. (However, this is compatible with this proposal and will likely be implemented in the future.)
- Alias names cannot contain the directory separators `/` and `\`.
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

Finally, providing support for alias maps within the Roblox engine is out of the scope of this RFC but is being considered internally.

### Caching

In the current implementation, required modules are cached based on the string that was used to require them. This means that using a mix of relative/absolute/aliased paths might lead to a cache miss—even if the module has already been required.

```lua
-- Requiring test.luau by absolute path
absolute = require("/Users/johndoe/luau-files/test")

-- Requiring test.luau by relative path
relative = require("test")

-- Currently prints false, module was re-required rather than loaded from cache
print(absolute == relative)
```

Currently, since relative paths are always evaluated in relation to the current working directory and absolute paths are always evaluated in relation to the system's root directory, all files in a project end up using the same paths in the current implementation. However, with the proposed change to resolving relative paths (in relation to the requiring file instead of in relation to the CWD), we might have files in different directories requiring modules using different relative paths.

For example, `require("mymodule")` and `require("../mymodule")` might refer to the same module, depending on the requiring files' locations. With the current cache implementation, the second statement would be a cache miss, as `"mymodule"` is not literally equal to `"../mymodule"`.

To solve this issue, we propose transforming every path that is passed to `require` into an equivalent absolute path and using this to cache, regardless of whether it was originally passed in as a relative, absolute, or aliased path. This way, a module's return value is stored in both a unique and consistent way across different files. Additionally, using absolute paths as opposed to, say, relative-to-cwd paths for caching ensures that aliased paths also maximize cache hits.

### Implementing changes to relative paths

The current implementation of evaluating relative paths (in relation to the current working directory) can be found in [lua_require](https://github.com/Roblox/luau/blob/e25de95445f2d635a125ab463426bb7fda017093/CLI/Repl.cpp#L133).

When reading in the contents of a module using [readFile](https://github.com/Roblox/luau/blob/e25de95445f2d635a125ab463426bb7fda017093/CLI/FileUtils.cpp#L47), the function `fopen`/`_wfopen` is called, which itself evaluates relative paths in relation to the CWD. In order to implement relative paths in relation to the requiring file, we have two options when evaluating a relative path.

**Assume the following:**
 - Current working directory: `"/Users/johndoe/project/subdirectory/cwd"`
 - Requiring file's location: `"/Users/johndoe/project/requirer.luau"`
 - Relative path given to require by user: `"./sibling"`

**Approach 1: Translate to the "correct" relative path**
 - Translated relative path given to `fopen`/`_wfopen`: `"../../sibling"`

**Approach 2: Convert the given relative path into its corresponding absolute path**
 - Translated relative path given to `fopen`/`_wfopen`: `"/Users/johndoe/project/sibling"`

Although `fopen`/`_wfopen` can handle both relative (to CWD) and absolute paths, the second approach makes more sense for our use case. We already need absolute paths for caching, as explained in the "Caching" section, so we might as well generate these absolute paths during the path resolution stage. With the first approach, we would need to call `realpath` to convert the relative-to-CWD path into an absolute path for caching, which is an unnecessary extra step.

However, for security reasons, we don't want to expose absolute paths directly to Luau scripts (for example, through `debug.info`). To prevent this, even though we will cache and read files by absolute path (which helps reduce cache misses), the `chunkname` used to load code [here](https://github.com/Roblox/luau/blob/e25de95445f2d635a125ab463426bb7fda017093/CLI/Repl.cpp#L152) will be the file's path relative to the current working directory. This way, the output of `debug.info` will be unaffected by this RFC's proposed changes.

- In the example above, the requiring file's stored `chunkname` would be `"../../requirer.luau"`, and its cache key would be `"/Users/johndoe/project/requirer.luau"` (was set when it was required).
- When resolving the path `"./sibling"`, we would apply this to `"../../requirer.luau"`, obtaining the relative-to-cwd path `"../../sibling.luau"`.
- This would become the `chunkname` of `sibling.luau` during loading.
- This would then be converted to an absolute path `"/Users/johndoe/project/sibling"` for caching by resolving it relative to the CWD.

In the case of an aliased path, it doesn't make sense to make the path relative to the CWD. In this case, the alias would remain in the `chunkname` to prevent leaking any absolute paths.

#### Where to begin traversal

One key assumption of this section is that we will have the absolute path of the requiring file when requiring a module by relative path.

While we could add an explicit reference to this directory to the `lua_State`, we already have an internal mechanism that allows us to get this information. We essentially want to call the `C++` equivalent of `debug.info(1, "s")` when we enter `lua_require`, which would return the name of the file that called `require`, or `stdin` if the module was required directly from the CLI.

As an example, we might do something like this in `lua_require`:

```cpp
static int lua_require(lua_State* L)
{
    lua_Debug ar;
    lua_getinfo(L, 1, "s", &ar);

    // Path of requiring file
    const char* basepath = ar.source;

    // ...
}
```

#### Impact on `debug.info` output

The current implementation also has a slight inconsistency that should be addressed. When executing a Luau script directly (launching Luau with a command-line argument: `"luau script.luau"`), that base script's name is internally stored with a file extension. However, files that are later required are stored with this extension omitted. As a result, the output of `debug.info` depends on whether the file was the base Luau script being executed or was required as a dependency of the base script.

For consistency, we propose storing the file extension in `lua_require` and always outputting it when `debug.info` is called.

### DataModel as VFS

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

#### Considerations

Within the Roblox engine, we will have to handle replication and waiting for modules to be loaded into the DataModel before requiring them. However, this is outside of the scope of this RFC and will be discussed internally.

## Drawbacks

### Backwards compatibility

Luau libraries are already not compatible with existing Lua libraries. This is because Lua favors the `.` based require syntax instead and relies on the `LUA_PATH` environment variable to search for modules, whereas Luau currently supports a basic require-by-string syntax.

- Libraries are fully compatible with the Roblox engine, as require-by-string is currently unimplemented.
- Luau currently implements relative paths in relation to the current working directory. 
- We propose changing this behavior and breaking backwards compatibility on this front.
- With the current implementation, requiring a library that itself contains relative-path require statements [can become a mess](https://github.com/Roblox/luau/issues/959) if the Luau VM is not launched from the "correct" working directory.
- We propose the following change: relative paths passed to require statements will be evaluated in relation to the requiring file's location, not in relation to the current working directory.
    - Caveat 1: if the relative-path require is executed directly in a REPL input prompt (not in a file), it will be evaluated relative to the pseudo-file `stdin`, located in the current working directory.
    - Caveat 2: if the code being executed is not tied to a file (e.g. using `loadstring`), executing require statements contained in this code will throw an error.
- If this causes issues, we can later introduce a default alias for the current working directory (e.g. `@CWD`).

## Alternatives

### Different ways of defining aliases

#### Defining paths/aliases directly in the requiring file

Rather than defining paths/alias maps in an external configuration file, we could alternatively define paths/aliases directly in the files that require them. For example, this could manifest itself through an extension of the `--!` comment syntax or introduce new syntax like `--@<ALIAS> = @<PATH>`.
```lua
--@"Roact" = @"C:/LuauModules/Roact-v1.4.2"
local Roact = require("@Roact")

-- Same as this:
local Roact = require("@C:/LuauModules/Roact-v1.4.2")
```

Some potential issues with this approach:
- Could lead to Luau file headers becoming cluttered.
- Would probably lead to substantial copy-and-pasting between modules in the same library, as they would likely need to share certain paths/aliases.
- Using configuration files for paths/alias maps allows modules to share aliases while still providing the flexibility to override if needed. This approach does not support inheritance and overriding in an obvious way.
- Removes the layer of abstraction that is provided by external paths/alias maps. This might also blur the scope of package managers. The software would need to directly modify lines of code in `.luau` files, rather than modifying configuration files.

#### Defining configuration files in a non-JSON format

Alias maps could alternatively be defined in specially named `.luau` files themselves, or at least adhering to Luau syntax. This approach is somewhat unappealing, however, as it would require package management software to parse Luau syntax. JSON syntax is well-understood and well-supported, which would likely faciliate the development of package management software.