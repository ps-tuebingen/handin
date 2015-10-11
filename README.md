This is the "handin" package for Racket, which provides infrastructure
for automated handin and checking of programs from within DrRacket.
Instructors use this infrastructure to generate a separate,
course-specific ".plt" file (or package) that students install.

Install the official version of the "handin" package with

  raco pkg install handin

or through the "Install Package..." menu item in DrRacket's "File"
menu.

To install this fork, run instead:

  raco pkg install https://github.com/ps-tuebingen/handin.git

After installing, see the "Handin Server" documentation that is added
to the Racket documentation.

## Branching

New code is developed in the `master` branch, either directly or in topic
branches.
- Using `merge-staging.sh`, you can pull code from `master` into the
  `deploy-staging` branch to test it onto the *staging server*.
- Using `merge-production.sh`, you can pull tested code from `deploy-staging`
  into the `deploy-production` branch to run it onto the *production server*.

No merging happens on the `deploy-*` branches, they are only
forwarded, so that `deploy-production` is always a non-strict subset of
`deploy-staging` which is a non-strict subset of `master`.

Please read through `merge-staging.sh` and `merge-production.sh`; they are
simple scripts to automate repetitive steps, not complex programs which handle
well unexpected conditions.
