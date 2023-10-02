# Require by String

## Summary

We need to add relative paths to `require` statements to facilitate the grouping together of related Luau files into libraries and allow for future package managers to be developed and integrated easily.

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

## Use cases

### Improvements to relative-path requires

By interpreting relative paths relative to the requiring file's location, Luau projects can now have internal dependencies. For example, in [Roact's current implementation](https://github.com/Roblox/roact), `Component.lua` requires `assign.lua` [like this](https://github.com/Roblox/roact/blob/beb0bc2706b307b04204abdcf129385fd3cb3e6f/src/Component.lua#L1C1-L1C45):

```lua
local assign = require(script.Parent.assign)
```

By using "Roblox-style" syntax (referring to Roblox Instances in the require statement), `Component.lua` is able to perform a relative-to-requiring-script require. However, with the proposed changes in this RFC, we could instead do this with clean syntax that works outside of the context of Roblox:

```lua
local assign = require("./assign")
```

(Of course, for this to work in the Roblox engine, there needs to be support for require-by-string in the engine. This is being discussed internally.)

### Paths array

The `paths` configuration variable provides convenience and allows Luau developers to build complex, well-organized libraries. Imagine the following project structure:

```
luau-paths-project
├── .luaurc
├── dependencies
│   └── dependency.luau
└── src
    └── module.luau
```

If `.luaurc` contained the following `paths` array:
```json
{
    "paths": ["./dependencies"]
}
```

Then, `module.luau` could simply require `dependency.luau` like this:
```lua
local dependency = require("dependency")

-- Instead of: require("../dependencies/dependency")
```

Using the `paths` array allows Luau developers to organize their projects however they like without compromising code readability.

### Large-scale projects in Luau

For large-scale Luau projects, we might imagine that every dependency of the project is a Luau project itself. We might use an organizational structure like this to create a clean hierarchy:

```
large-luau-project
├── .luaurc
├── subproject-1
├── subproject-2
└── subproject-3
```

We can provide the following alias in `large-luau-project/.luaurc`:

```json
{
    "aliases": {
        "com.roblox.luau": "."
    }
}
```

This way, each subproject directory can contain its own source code, dependencies, and `.luaurc` configuration files, while also inheriting the `com.roblox.luau` alias from `large-luau-project/.luaurc`.

This allows us to refer to other subprojects like this, regardless of the exact location of the requiring file in `large-luau-project`:
```lua
local subproject1 = require("com.roblox.luau/subproject-1")
```

### Roblox specifics

In the Roblox engine, developers can reasonably expect something similar to be implemented. Assuming a central package management system were available for storing packages and providing default aliases to require them, a Roblox Script could contain `local Roact = require("Roact")`, and everything would "just work".

However, when requiring a "floating" ModuleScript (not part of this central package management system), there will not be a default alias provided. This is bad style and will not be encouraged.

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