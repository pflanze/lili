# Lili - an interpreter (or set of interpreters) for Scheme and similar languages

This project's aims are two-fold: on one hand the aim is to run Scheme
(and potentially similar languages like JavaScript) in various
environments with full support for runtime typing and interactive
development, first-class continuations and tail-call optimization, and
good debugging. On the other hand, to compile languages based on
s-expressions and access to Scheme in the macro level to the host
language of the targeted environment (JavaScript, C, perhaps C++,
Rust), in a way that does not offer these features. It is the aim to
make use of the latter for parts of the implementation of the former.

See [plan](docs/plan.md) and [javascript-plan](docs/javascript-plan.md).
