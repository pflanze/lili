# Plan specifically via JavaScript

To get to work code in JavaScript as quickly as possible, this plan
can be followed:

1.  like in [plan](plan.md), implement core data structures (like
    booleans, numbers, strings, symbols, vectors, structures/objects),
    but in JavaScript.

1.  Scheme parser, similar to [plan](plan.md), but write it in
    Scheme. Use explicit calls to contructors for Scheme data types.

1.  make translator from Scheme to JavaScript, without supporting TCO,
    call/cc, or all the Scheme data types; translate the constructors
    for the Scheme data types to calls to the JavaScript functions
    from above.

1.  Increase scope of previous point to allow for all features wished
    to be had quickly (interoperation with JavaScript, language features).

1.  continue with [plan](plan.md).

