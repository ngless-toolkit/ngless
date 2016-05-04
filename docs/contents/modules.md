# Modules

## External Modules

External scripts can perform two tasks:

1. Add new references to ngless
2. Add functions to ngless

### New references

A module can add references to ngless which can then be used in the `map()`
call using the `reference` argument.

Like everything else in ngless, these are versioned for reproducibility so that
the resulting script implicitly encodes the exact version of the databases used.

### New functions

An external module is a way to have functions in ngless map to command line
calls to your scripts.

## Internal Modules

This is very advanced as it requires writing Haskell code which can then
interact very deeply with the rest of ngless.

