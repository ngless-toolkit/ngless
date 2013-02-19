===================
Structure of ngless
===================

This document describes the implementation, not the language.


Since the scripts are assumed to be small, everything is performed in memory.

1. The whole text is loaded into memory (after UTF-8 decoding).
2. A complete abstract syntax tree is built.
3. The syntax tree is validated. This includes several checks for sanity.
4. The syntax tree is optimised. This step is not currently implemented.
5. The syntax tree is interpreted.

