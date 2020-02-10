# legovogel

A work in progress - help me come up with a better name. Monorail, legovogel (German: Bird made out of lego), tummut (Finnish: tarnish).

Monorail is mostly a direct descendant of [railway](https://github.com/jndean/railway), but this time around I am:

- Writing a 'proper' bytecode compiler and virtual machine, as opposed to the old tree-walking interpreter which inherited directly from the abstract syntax tree.
- Trying out a more mature ownership system, where aliasing is now possible but the language can still prevent [self-modification](https://github.com/jndean/railway/wiki/Variables,-Data-and-Scope#self-modification-and-aliasing) during static analysis.
- Learning Rust.
- Probably not bothering with multi-threading support.
- Making the syntax _slightly_ prettier.

