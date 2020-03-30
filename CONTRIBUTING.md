# Contributors Guide

## Bug Reports

Please [open an issue](https://github.com/emilypi/possibly-can/issues/new) if you have a bug to report.

The more detailed your report, the faster it can be resolved and will ensure it
is resolved in the right way. I personally appreciate it when people not only open
issues, but attempt to resolve them on their own by submitting a pull request. I am
always open to constructive feedback, and I am by no means an expert, so guidance
should always be considered welcome.

## Documentation

If you would like to help with documentation, please remember to update any and
all module headers with the appropriate copyright dates, ranges, and authorship.

Expansions to the documentation are welcome, and appreciated. Every small
contribution counts.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve `possibly-can`, pull requests are most welcome. It's a good idea to
[submit an issue](https://github.com/emilypi/possibly-can/issues/new) to
discuss the change before plowing into writing code.

Any and all claims of "performance" should be backed up with benchmarks. You can
add them to the existing benchmark suite in your PR, as long as you do not make
unjustifiable changes to the existing code.

## Code Quality

The `possibly-can` project intends to focus on 4 things as primary objectives: generality, simplicity, power,
and modularity. As such, all code should strive to maintain
a balance between the latter 3 that keeps in mind the end-result to the user. Ask yourself
the following questions:

1. If the code is user facing, does this code necessarily complicate the UX?
2. If the code is not user facing, do we obtain an overall win when it comes to the
   power, generality, modularity when compared to the simple alternative? What is the
   cognitive overhead of maintaining such a construct?
3. Is my code necessary, or is it a convenient refactoring that makes more sense in terms
   of my personal understanding of the code?

In the case of 1., the end user is going to be writing configuration scripts. There is no
need to overcomplicate the experience. We must obviously satisfy the spec, but any
conveniences in addition to the spec must be thought through.

In the case of 2., if code is not user-facing, we must consider the maintenance burden
of additional cognitive overhead vs. a simpler alternative. There is no hard and fast
rule here; it is up to taste. Please discuss with us before wasting time on this.

In the case of 3., boy do Haskellers love to refactor for no reason! I enjoy it as well,
but please understand that this is a production-grade library, and refactors always
increase the surface area for bugs in a multiplicative way. Bugs combine with bugs.
Restrain yourself!


## Testing

All new code should be covered by tests. Additionally, if you find uncovered material
in the existing codebase, tests are always appreciated as pull requests.
