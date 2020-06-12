# Sandboxing

Luau is safe to embed. Broadly speaking, this means that even in the face of untrusted (and in Roblox case, actively malicious) code, the language and the standard library don't allow any unsafe access to the underlying system, and don't have any bugs that allow escaping out of the sandbox (e.g. to gain native code execution through ROP gadgets et al). Additionally, the VM provides extra features to implement isolation of privileged code from unprivileged code and protect one from the other; this is important if the embedding environment (Roblox) decides to expose some APIs that may not be safe to call from untrusted code, for example because they do provide controlled access to the underlying system or risk PII exposure through fingerprinting etc.

This safety is achieved through a combination of removing features from the standard library that are unsafe, adding features to the VM that make it possible to implement sandboxing and isolation, and making sure the implementation is safe from memory safety issues using fuzzing.

Of course, since the entire stack is implemented in C++, the sandboxing isn't provable - in theory, compiler or the standard library can have exploitable vulnerabilities. In practice these are usually found and fixed quickly. While implementing the stack in a safer language such as Rust would make it easier to provide these guarantees, to our knowledge (based on existing code) this would make it impossible to reach the level of performance required.

## Library
