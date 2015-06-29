## tozero

```
tozero variable {
    code
}
```

Runs the loop `variable` amount of times.

The first iteration leaves `variable` unaltered, and the last iteration leaves
`variable` at 1. After the loop, `variable` is set to 0.

`variable` has the type of either `X`, `Y`, or a memory address.

If the set of `variable` is a subset of `(1..255)`, an initial zero-check isn't required.
Inside the loop, the set of `variable` is intersected with `(1..255)`.

## toneg

```
toneg variable {
    code
}
```

Runs the loop `variable` + 1 amount of times.

The first iteration leaves `variable` unaltered, and the last iteration leaves
`variable` at 0. After the loop, `variable` is set to 255.

`variable` has the type of either `X`, `Y`, or a memory address.

If the set of `variable` is a subset of `(0..127)`, an initial negative-check isn't required.
Inside the loop, the set of `variable` is intersected with `(0..127)`.
