# Release process

## Releasing

The release process is automated thanks to:
- https://github.com/djspiewak/sbt-github-actions#integration-with-sbt-ci-release
- https://github.com/olafurpg/sbt-ci-release

To release, push a git tag:

```
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```
Note that the tag version MUST start with `v`.

Wait for the CI pipeline to release the new version. Publishing the artifacts on maven central can take time.

## Updating the release notes

Open the [releases](releases). A draft should already be prepared.

Edit the draft release to set the released version. Complete the release notes if necessary. And save it.
