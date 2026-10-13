#!/usr/bin/env bash
# Runs all JMH benchmarks and writes the results to modules/benchmarks/RESULTS.md.
# Extra args are passed to JMH, e.g. ./run-benchmarks.sh -i 2 -wi 2 -f1
set -euo pipefail
cd "$(dirname "$0")/../.."
json=$(mktemp)
sbt "sangria-benchmarks/Jmh/run -rf json -rff $json $*"
{
  echo "# Benchmark results"
  echo
  echo "Generated: $(date -u +%Y-%m-%dT%H:%MZ), commit $(git rev-parse --short HEAD), $(java -version 2>&1 | head -1)"
  echo
  echo "| Benchmark | Params | Score | Error | Unit |"
  echo "|---|---|---:|---:|---|"
  jq -r '.[] | "| \(.benchmark | sub("sangria.benchmarks.";"")) | \(.params // {} | to_entries | map("\(.key)=\(.value)") | join(", ")) | \(.primaryMetric.score * 1000 | round / 1000) | \(.primaryMetric.scoreError | tostring | .[0:8]) | \(.primaryMetric.scoreUnit) |"' "$json"
} > modules/benchmarks/RESULTS.md
