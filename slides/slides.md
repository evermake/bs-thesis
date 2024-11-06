---
theme: seriph
background: false
---

<h1 class="text-4xl!">
  Hindley-Milner-style Type Inference
  <br/>
  with Subtyping via Second-Order Abstract Syntax
</h1>

<div class="mt-8 text-gray-4">
Vladislav Deryabkin, Track — BS21, Supervisor — Nikolai Kudasov
</div>


---

# Context

<span/>

Type system — is a crucial component for any modern programming language:

- Errors are detected earlier.
- Types serve as a documentation.
- IDE hints help developers.

<br/>

Most popular programming languages did not have types<v-click>, but they do now:</v-click>

<div class="grid grid-cols-2 gap-4">

````md magic-move {at:'1'}
```python
# Python
def fib(n):
    a, b = 0, 1
    while a < n:
        yield a
        a, b = b, a+b
ㅤ
ㅤ
```
```python
# Python with MyPy
def fib(n: int) -> Iterator[int]:
    a, b = 0, 1
    while a < n:
        yield a
        a, b = b, a+b
ㅤ
ㅤ
```
````

````md magic-move {at:'1'}
```javascript
// JavaScript
function fib(n) {
  let a = 0, b = 1;
  while (a < n) {
    yield a;
    [a, b] = [b, a + b];
  }
}
```
```typescript
// TypeScript
function fib(n: number): Generator<number> {
  let a = 0, b = 1;
  while (a < n) {
    yield a;
    [a, b] = [b, a + b];
  }
}
```
````

</div>

---

# Problem

<span/>

Implementing a type system for a programming language is **hard**.

<hr/>

Some popular strongly-typed languages and theory their typecheckers based on:

- Rust: <span class="text-gray" v-mark.underline.orange="1">Hindley-Milner Type Inference</span>
- TypeScript: <span class="text-gray">Structural Typing</span>
- Haskell: <span class="text-gray" v-mark.underline.orange="1">Hindley-Milner Type Inference</span>
- C++: <span class="text-gray">Nominal Subtyping</span>
- Java: <span class="text-gray">Nominal Subtyping</span>
- Swift: <span class="text-gray">Nominal Subtyping</span>
- Scala: <span class="text-gray" v-mark.underline.orange="1">Hindley-Milner Type Inference</span>

---

# Goals

<span/>

1. Generalize Hindley-Milner-style type inference algorithm and extract it to a Haskell library,
allowing users to "outsource" the implementation of the core of their typechecking.

```haskell
import qualified HindleyMilner.AlgorithmW as TypecheckingAlgorithm

-- Efficiently typecheck the user program.
typecheck :: AST n -> Bool
typecheck ast = TypecheckingAlgorithm.typecheck customRules ast
  where
    customRules = ...
```

<hr class="my-6"/>

2. Implement a language showcasing the benefits and functionality of the new library.

---

# Related Works

- Free Foil — ...
- ...

---

# Preliminary Results

<span/>

We've created a simple language based on lambda calculus using Haskell and FreeFoil.

Language already supports:
- Boolean and integer literals;
- If-then-else expressions;
- Let-bindings;
- Lambda-abstractions (functions) and applications;
- Hindley-Milner style type inference.

`example.lam`

```
let f = λx. x + 1
  in f true
```

``` sh
free-foil-hm example.lam
# Typechecking error: cannot unify (Nat with Bool)
```

---

# Plan of Work

1. Finish implementation for the concrete language and verify it is correct.
2. Choose and generalize unification algorithm, extract it to the library.
3. Implement a language showcasing the benefits and functionality of the new library.
4. Compare and benchmark results with other approaches of implementing typechecking.

---

# Acknowledgments

I have been working with these people who I am grateful for their huge contribution:
- Nikolai Kudasov
- Diana Tomilovskaya
- Anastasia Smirnova

<br/>

# Materials

You can find slides and current results of my work in the GitHub repository:<br/>
[github.com/evermake/diploma](http://github.com/evermake/diploma)

---

# References

- A Theory of Type Polymorphism in Programming, _Robert Milner_ (1978)
- Free Foil: Generating Efficient and Scope-Safe Abstract Syntax, _Nikolai Kudasov, et al._ (2024)
- Hindley–Milner type system, _Wikipedia_ (2024)

---
layout: center
---

# Thank you.
