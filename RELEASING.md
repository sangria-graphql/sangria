# How to perform a release

## check for backwards compatibility

- check the value of `mimaPreviousArtifacts` in `build.sbt` if necessary
- in sbt:
```
++2.12.10 // if the previous artifact is only present for scala 2.12
sangria-core/mimaFindBinaryIssues
```

## Prepare the release

```
sbt release
```
This should prepare artifacts for all scala versions on [sonatye Nexus](https://oss.sonatype.org/)

## Publish the artifacts

- Login on [sonatye Nexus](https://oss.sonatype.org/)
- Open the [staging repositories](https://oss.sonatype.org/#stagingRepositories)
- Find the freshly created artifacts
- Close the repository
- Release it
