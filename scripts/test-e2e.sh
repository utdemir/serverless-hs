#!/usr/bin/env bash

set -o xtrace
set -o pipefail
set -o errexit
set -o nounset

cd $(dirname "${BASH_SOURCE[0]}")/../

tmpdir=$(mktemp -d)
function cleanup() {
    rm -rf $tmpdir
}
trap cleanup EXIT

stack install --local-bin-path $tmpdir/

stack install --stack-yaml examples/stack.yaml
cp examples/.stack-work/docker/_home/.local/bin/simple $tmpdir

cd $tmpdir

cat <<EOF > test.yaml
region: us-east-1
stack_name: serverless-hs
bucket_name: serverlesshs
EOF

./serverless-hs simple test.yaml



