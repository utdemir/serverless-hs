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

stack install --stack-yaml examples/stack.yaml --no-docker --local-bin-path $tmpdir/
mv $tmpdir/simple $tmpdir/hs-main
cp static/handler.js $tmpdir/

cd $tmpdir

cat <<EOF > call.js
var handler = require("./handler.js").handler;

handler(1, [], (ans, cb) => {
  console.log("Got answer: " + ans + " " + JSON.stringify(cb))
});
EOF

SERVERLESS_HS_FUNCTION_ID=meta node call.js


