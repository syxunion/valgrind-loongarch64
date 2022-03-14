# Valgrind - LOONGARCH64/Linux

Valgrind with support for the LOONGARCH64/Linux platform.

## Note

This is an experimental project in early stage.

I may use `git push -f` when necessary.

Please come back later.

The repository is at this point made public only to avoid duplicating effort should someone else be also interested working on this support.

## Current State

```text
== 642 tests, 15 stderr failures, 0 stdout failures, 2 stderrB failures, 0 stdoutB failures, 1 post failure ==
gdbserver_tests/hginfo                   (stderrB)
gdbserver_tests/mcblocklistsearch        (stderrB)
gdbserver_tests/mcclean_after_fork       (stderr)
gdbserver_tests/mcsignopass              (stderr)
gdbserver_tests/mcsigpass                (stderr)
gdbserver_tests/mcwatchpoints            (stderr)
memcheck/tests/leak-cases-full           (stderr)
memcheck/tests/leak-cases-summary        (stderr)
memcheck/tests/leak-cycle                (stderr)
memcheck/tests/leak-segv-jmp             (stderr)
memcheck/tests/leak-tree                 (stderr)
memcheck/tests/lks                       (stderr)
memcheck/tests/vcpu_fbench               (stderr)
memcheck/tests/vcpu_fnfns                (stderr)
helgrind/tests/tls_threads               (stderr)
drd/tests/pth_barrier_thr_cr             (stderr)
drd/tests/pth_mutex_signal               (stderr)
massif/tests/big-alloc                   (post)
```

## Known Issues

- I'm not sure how to deal with glibc errors and just ignore them for now.
- Need to add tests for Memcheck and Nulgrind (see `/* TODO */`).
