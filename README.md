# Eitri - A low-overhead Functional Language that compiles to C++

Eitri is a functional language with a human-readable C++ target, aiming to use high-level functional techniques to produce efficient, low-overhead C++ code with low-cost abstractions.

```
def (a : int, b: string) =
  println(b)
  val c = a * 2
  c + 2
```
->
```
auto foo(int a, string b) {
  println(b);
  const auto c = a * 2;
  return c + 2;
}
```
