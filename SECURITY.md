# Security Guarantees

Luau provides a safe sandbox that scripts can not escape from, short of vulnerabilities in custom C functions exposed by the host. This includes the virtual machine, builtin libraries and native code generation facilities.

Any source code can not result in memory safety errors or crashes during its compilation or execution. Violations of memory safety are considered vulnerabilities.

Note that Luau does not provide termination guarantees - some code may exhaust CPU or RAM resources on the system during compilation or execution.

The runtime expects valid bytecode as an input. Feeding bytecode that was not produced by Luau compiler into the VM is not supported, and
doesn't come with any security guarantees; make sure to sign and/or encrypt the bytecode when it crosses a network or file system boundary to avoid tampering.

# Reporting a Vulnerability

You can report security bugs via [HackerOne](https://hackerone.com/roblox). Please refer to the linked page for the rules of the bounty program, which defines scope and criteria for Roblox.

Alternatively, you can report issues privately through [GitHub Security Advisories](https://github.com/luau-lang/luau/security/advisories/new) page.
Note that reports submitted through GitHub are not eligible for bounty rewards of the HackerOne program.

## Scope

The areas listed in this section are explicitly out of scope.

We reserve the right to tell you when a report should be opened as a regular issue and update this list with new cases.

### Custom bytecode provided to `luau_load`

The runtime assumes that bytecode is produced by Luau compiler and is not designed to consume arbitrary input.
Please see the [sandboxing documentation](https://luau.org/sandbox/#library) for details.
Embedders are responsible for ensuring the bytecode that is being loaded has been produced by the Luau compiler.

### Features under a Debug* flag

Features enabled by flags starting with Debug* are experimental and in development.
Open a regular issue if you found a bug under such a flag.

### Stack overflows

Stack size is a parameter of an environment which might not be configured to an appropriate size by the embedder and also highly dependant on the build options.
We do not consider these to be security vulnerabilities and will ask you to open a regular issue.

### Low severity Analysis/ issues

Resource exhaustion and null pointer access crashes in the type checking should be reported as regular issues.
