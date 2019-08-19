This is a README about the code, i.e., the implementation of ngless. For user
documentation (i.e., how to use ngless), see https://ngless.embl.de


# Code structure

Start by the comments on the top of Main.hs and follow the pointers there.

`Utils` contains a random grab bag of utility functions, which are mostly
self-contained except for relying on NGError.

## Conventions


### GHG Extensions

NGLess is not against relying on GHC extensions. The following extensions are
turned on globally (ordered by how important they are):

 - OverloadedStrings
 - LambdaCase
 - BangPatterns
 - TupleSections

A few others are turned on file by file.

### imports

The following are conventional imports:

    import qualified Data.ByteString as B
    import qualified Data.ByteString.Char8 as B8
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Lazy.Char8 as BL8

    import qualified Data.Text as T

    import Data.Conduit as C
    import qualified Data.Conduit.List as CL

Standard modules (such as `Data.Maybe`) are imported unqualified. This should
only be done for modules that are part of ``base``.


### Tests

Tests come in two forms:

1. unit tests (start in Tests.hs)
2. behaviour tests (not in the code, see the tests/ directory).

All else being equal, unit tests are prefered as they run much faster (a single
invocation runs all of them). Behavioural tests are better at testing the whole
programme and often easier to write.

