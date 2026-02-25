# scalacf-zio

Streaming arithmetic for continued fractions (Gosper / HAKMEM 101B), rewritten as a clean ZIO + ZIO Test codebase.

## Modules

- **cf-core**: production library (continued fractions, ranges, transforms, engines)
- **cf-test**: ZIO Test suites + JSON vector runner

## Goals

- Preserve the proven algorithms from the legacy ScalaCF codebase
- Rebuild “from first principles” with:
  - ZIO Test as the primary harness
  - bounded / diagnostic execution paths (DfT)
  - explicit contracts (pre/postconditions, no nulls)

## Running tests
