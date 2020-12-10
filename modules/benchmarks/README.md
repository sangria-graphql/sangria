# benchmarks for sangria

The benchmarks are run with [OpenJDK JMH](http://openjdk.java.net/projects/code-tools/jmh/) and integrated in sbt with [sbt-jmh](https://github.com/ktoso/sbt-jmh)

When developing a new benchmark:

```
jmh:run -i 1 -wi 1 -f1 -t1
```

where:

    -i <int>                    Number of measurement iterations to do.
    -wi <int>                   Number of warmup iterations to do.
    -f <int>                    How many times to forks a single benchmark.
    -t <int>                    Number of worker threads to run with.

Other options can be found with
```
jmh:run -h
```

For a real benchmark:
```
jmh:run
```
