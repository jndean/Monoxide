# Monoxide

A work in progress - Monoxide is mostly a direct descendant of my other reversible imperative programming language [Railway](https://github.com/jndean/railway), but this time around I am:

- Writing a 'proper' bytecode compiler and virtual machine, as opposed to the old tree-walking interpreter which inherited directly from the abstract syntax tree.
- Trying out a more mature ownership system, where aliasing is now possible but the language can still prevent [self-modification](https://github.com/jndean/railway/wiki/Variables,-Data-and-Scope#self-modification-and-aliasing) during static analysis.
- Learning Rust.
- Probably not bothering with multi-threading support.
- No longer making it the interpreter's mandate to catch non-reversible behaviours that are only detectable *at runtime*.
- Making the syntax _slightly_ prettier.



### References

One of the key ways Monoxide differs from Railway as a language if the ability to create references.

```Monoxide
x := 8;
y := &x;
y += 1;
x =: 9;
```

References can cross scope boundaries, so for example the following function which gets a reference to the first item in an array is possible.

```Monoxide
fn get_first(&a array)() 
{
    first := &array[0];
} 
~get_first(&a first)
```

Additionally for loops can not work by iterating a reference to the contents of arrays, unlike Railway which required the for loop to copy elements during iteration to avoid aliasing.

```Monoxide
X := [1, 2, 3, 4];
for (x in X) {
    x /= 2;
}
X =: [1/2, 1, 3/2, 2];
```

As mentioned, these new references are safe because the syntax checker can statically track them and throw compile-time errors when self-modification is possible.

```Monoxide
x <= get_first(X);
X[0] += x;              $ Not reversibe, won't compile! $

for (x in X) {
    X[x] += 1;          $ Not reversible, won't compile! $ 
}
```





