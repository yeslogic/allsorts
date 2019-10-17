# Contributions Welcome

Allsorts is already powering font loading and shaping in Prince, but there's
still plenty more to implement. We welcome contributions of many forms including
code, documentation, test cases, and language specific guidance.

Follow the [contribution workflow](#workflow) for submitting
changes to the codebase. Or raise an issue to report a bug, or discuss a
possible change.

## Workflow

Follow these steps to contribute to the project:

1. Within your fork, create a branch for your contribution. Use a meaningful
   name.
1. Create your contribution, meeting all
   [contribution quality standards](#quality-standards).
1. Ensure all the tests pass (`cargo test`).
1. [Create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
   against the `master` branch of the repository.
1. Once the pull request is reviewed and CI passes, it will be merged.

## Quality Standards

Most quality and style standards are checked automatically by the CI build.
Contributions should:

- Separate each **logical change** into its own commit.
- Include tests for any new functionality in your pull request.
- Document public functions.
- Format code with `cargo fmt`.
- Avoid adding `unsafe` code. If it is necessary, provide an explanatory comment
  on any `unsafe` block explaining its rationale and why it's safe.
- Add a descriptive message for each commit. Follow [these commit message
  guidelines](https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
- Document your pull requests. Include the reasoning behind each change, and
  the testing done.
