# Types

Fn65 is a statically-typed, strongly-typed language.

No lossy coercion is possible without unsafe code.


## Registers and flags are types

* The following are 6502 registers: `A`, `X`, `Y`
* The following are 6502 flags: `NF`, `ZF`, `CF`, `IF`, `DF`, `VF`

Fn65 solves the issue of _type ownership_. A variable with a register or flag
type has _exclusive ownership_ of the register or flag.

In other words, two variables with the same singleton type cannot be
active at a time. The following will fail to compile:

```
fn foo(old_a: A) -> A {
    // transfer ownership of singleton `A` to `new_a`.
    let new_a: A = 5;

    // compiler error.
    // `old_a` is irrelevant; `new_a` contains the accumulator value.
    old_a + 1
}
```

In 6502 assembly, it's easy to unknowingly overwrite a register or flag that you
were using, or to access the wrong register or flag.
This mistake is easy to make at call-site, or in complicated loops and branches.
In Fn65 however, this is a compiler error.

6502 (cheerily compiles):
```
; Y = bytes to copy
; src = bytes to copy from
; dest = bytes to copy to
;
; After invocation: Y = 255, flag N = 1, flag Z = 0
; Doesn't affect flags: I, D, V
; Doesn't affect register: X
memcpy:
    DEY
    BMI .end
    LDA (src), Y
    STA (dest), Y
    DEY
    JMP memcpy
.end:
    RTS

; Copies 16 bytes from $0500 to $081F
uses_memcpy:
    LDA #$00
    STA src
    LDA #$05
    STA src

    LDA #$1F
    STA dest
    LDA #$08
    STA dest

    LDX #16
    JMP memcpy
```

Fn65 (frowningly fails):
```
fn memcpy(dest: *u8, src: *u8, bytes: Y+) {
    let offset = bytes - 1;

    toneg offset {
        dest[offset] = src[offset];
        offset = offset - 1;
    }
}

fn uses_memcpy() {
    let src: *u8 = 0x0500:A;
    let dest: *u8 = 0x081F:A;
    let bytes: X+ = 16;

    // compiler error: memcpy requires `bytes` to be of type `Y+`.
    memcpy(dest, src, bytes);
}
```


## Sets

All numeric types can be annotated with a set. A set defines all valid values
of the type.

A set can be a range, a composition, or comma-delimited.

An empty set is a compiler error.

* All: `X(0..255)`, `X(*)`, `X`
* Non-zero: `X(1..255)`, `X(* - 0)`
* Positive signed: `X(1..127)`, `X+`
* Negative signed: `X(128..255)`, `X-`
* Nibble: `X(0..15)`
* Zero: `X(0)`
* One: `X(1)`
* Even nibbles: `X(0, 2, 4, 6, 8, 10, 12, 14)`, `X(even . 0..15)`, `X(mul 2 . 0..7)`
* Odd nibbles: `X(1, 3, 5, 7, 9, 11, 13, 15)`, `X(odd . 0..15)`, `X(add 1 . mul 2 . 0..7)`

Arithmetic operations will remap the set of a type:
```
fn decrement_even(value: Y(0, 2, 4, 6)) -> Y(1, 3, 5, 7) {
    value - 1
}

fn decrement_positive(value: Y+) -> Y(0..254) {
    value - 1
}

fn double(value: A(0..3)) -> A(0, 2, 4, 6) {
    value << 1
}

fn halve(value: A(0, 2, 4, 6)) -> A(0..3) {
    value >> 1
}
```

### Sets and branches

If multiple branches end in a type with different sets, those sets will be unioned.

```
fn maybe_double(flag: ZF, value: Y(0..3)) -> Y(0,1,2,3,4,6) {
    if flag {
        // BTW, flag here has the type ZF(1)

        ((value:A) << 1):Y
    } else {
        // and flag here has the type ZF(0)
        value
    }
}
```

On a condition that entails a subset, the set will have this effect in the "on true" and "on false" code.

```
fn stuff(value: A) -> A(mul 2 . 0..15) {
    if value < 16 {
        // value has the type A(0..15)
        double_nibble(value)
    } else {
        // value has the type A(16..255)
        0:A
    }
}

fn double_nibble(value: A(0..15)) -> A(mul 2 . 0..15) {
    value<<1
}
```

### Set coercion rules

Given the assignment `a = b`:

* The set of `a` must be a superset of the set of `b`.
* Equivalently, the set of `b` must be a subset of the set of `a`.
* Equivalently, every element in the set of `b` must be in the set of `a`.

### Set cast rules

Given the assignment `a = b:A` (notice the lack of a set):

* Despite the lack of an explicit set in `A`, it inherits the same set as the set of `b`.

### Unsafe set coercion

It's sometimes necessary or desired to coerce a type into a non-superset, i.e.
if you're dealing with hardware registers or upholding other invariants.

Set checking is turned off inside the `unsafe` block:

```
unsafe { a = b };
```

_Be warned_: If the actual value in `a` is not in the set of `a`, the program
may invoke undefined behavior.
