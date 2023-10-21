# active_oberon

This is the start of a multi-language compiler for the Oberon family. A single compiler will be able to compile source code for Oberon, Oberon-2, and ActiveOberon. Starting with a parser for the latter one at first.

This compiler written in Rust will be able to execute under Linux, Windows, and Mac and generate code for X86-64 and ARM V8.2, even in cross-compiler mode and with projects with source code in different dialects of Oberon generating a single executable file.

The rest of the system, utilities, and libraries will be entirely written in ActiveOberon and will be stage two of building from the git repository.

The project has just started and will not be useful for much until the compiler is more done.
