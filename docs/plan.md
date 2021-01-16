# Plan

This is with a view of targeting C. Targeting JavaScript would be
similar and can be done in parallel (of course point 2 would not be
needed, except when targeting WASM in which case C may be used as an
intermediary). Also see [javascript-plan](javascript-plan.md).

1.  core data structures (like booleans, numbers, strings, symbols,
    vectors, structures/objects)

2.  garbage collector (I want to implement a real-time GC, will have
    to dig into some papers)

3.  Scheme parser, I already have one (in C++) but it's a bit ugly in
    places, and it uses the machine stack which can lead to stack
    overflows which is bad; thus, partially take the existing one and
    port and improve it, or write a new one from scratch. (Note: point
    3 on [javascript-plan](javascript-plan.md) will develop a
    translator from an s-expression based language to JavaScript and
    that could be extended to output C instead; so the hand-written C
    based parser might be temporary only, and the attention may
    primarily be on a subset of the necessary features just for
    learning how the parser works, and to learn how the generated code
    should look.)

4.  the s-expression->core frontend I mentioned above, I'm currently
    working out the algorithms in Scheme.

5.  define the core AST (abstract syntax tree), which represents the
    elements of the core language. (Probably CPS/SSA based; look into
    stack based as an alternative.)

6.  interpret the AST directly, or make a translator to byte code,
    make a byte code loader and interpreter.

7.  implement debugger (based on the continuation support)

