# Integration tests for ngless

Each directory is a test. There should be an ngless file (identified by the
extension `.ngl`, the actual filename is irrelevant), which is run.

## Normal tests

A file with extension `.ngl` is run through ngless. For a successful test, the
following elements are checked:

- The `ngless` process completed without errors (see _Error tests_ below for
  tests that should fail).
- For any file matching `expected.*`, a corresponding `output.*` should exist
  with the same content as the expected file. For example, if the test includes
  `expected.my.file.sam` then running the ngless script needs to create a file
  `output.my.file.sam` with the same contents.
- If there is a file called `check.sh` it is run and it should return 0.

### Cleanup

After running the test, temporary files and `output.*` are removed. If it
exists, the script `cleanup.sh` is run.
  
## Error tests

Tests that start with `error-` are "error tests", i.e., ngless **should fail**
on these. Tests starting with `error-validation-` should fail on validation
(i.e., when ngless is run with `-n` on the command line).


