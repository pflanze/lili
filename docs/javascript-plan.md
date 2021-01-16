# Plan specifically via JavaScript

To get to work code in JavaScript as quickly as possible, this plan
can be followed:

1.  like in [plan](plan.md), implement core data structures (like
    booleans, numbers, strings, symbols, vectors, structures/objects),
    but in JavaScript.

2.  Scheme parser, similar to [plan](plan.md), but write it in
    Scheme. Use explicit calls to contructors for Scheme data types.

3.  make translator from Scheme to JavaScript, without supporting TCO,
    call/cc, or all the Scheme data types (let's tentatively call this
    language *I* or ilang); translate the constructors for the Scheme
    data types to calls to the JavaScript functions from above.

4.  Increase scope of previous point to allow for all features wished
    to be had quickly (interoperation with JavaScript, language
    features).  (It would still not support features like TCO or
    call/cc, because JavaScript doesn't offer them, and the JavaScript
    debugger has to be used for debugging, but the generated
    JavaScript will be pretty readable.)

5.  continue with [plan](plan.md), which will provide the
    infrastructure to interpret (or as additional step compile) the
    remaining Scheme features (TCO, call/cc, debugger).

