# Monoxide

A work in progress - Monoxide is mostly a direct descendant of my other reversible imperative programming language [Railway](https://github.com/jndean/railway), but this time around I am:

- Writing a 'proper' bytecode compiler and virtual machine, as opposed to the old tree-walking interpreter which inherited directly from the abstract syntax tree.
- Trying out a more mature ownership system, where aliasing is now possible but the language can still prevent [self-modification](https://github.com/jndean/railway/wiki/Variables,-Data-and-Scope#self-modification-and-aliasing) during static analysis.
- Learning Rust.
- Probably not bothering with multi-threading support.
- No longer making it the interpreter's mandate to catch non-reversible behaviours that are only detectable *at runtime*.
- Making the syntax _slightly_ prettier.



### References

Thanks to the new ability to create references, functions like the following are now possible

```Monoxide
fn get_first_ref(&a array)() 
{
    first := &array[0];
} 
~get_first_ref(&a first)
```

Additionally for loops can iterate a reference to the contents of arrays, unlike Railway which required the for loop to copy elements during iteration to avoid aliasing.

```Monoxide
X := [1, 2, 3, 4];
for (x in X) {
	x += 1;
}
X =: [2, 3, 4, 5];
```

As mentioned, these new references are safe because the syntax checker can statically track where them and throw compile-time errors when self-modification is possible.

```Monoxide
x <= get_first_ref(X);
X[0] += y;              $ Won't compile! $

for (x in X) {
    X[x] += 1;          $ Won't compile! $ 
}
```





