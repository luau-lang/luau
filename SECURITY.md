# Security Guarantees

Luau provides a safe sandbox that scripts can not escape from, short of vulnerabilities in custom C functions exposed by the host. This includes the virtual machine, builtin libraries and native code generation facilities.

Any source code can not result in memory safety errors or crashes during its compilation or execution. Violations of memory safety are considered vulnerabilities.

Note that Luau does not provide termination guarantees - some code may exhaust CPU or RAM resources on the system during compilation or execution.

The runtime expects valid bytecode as an input. Feeding bytecode that was not produced by Luau compiler into the VM is not supported, and
doesn't come with any security guarantees; make sure to sign and/or encrypt the bytecode when it crosses a network or file system boundary to avoid tampering.

# Reporting a Vulnerability

You can report security bugs via [HackerOne](https://hackerone.com/roblox). Please refer to the linked page for rules of the bounty program.
