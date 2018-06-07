#!/bin/bash -e
export SCRIPT_HOME=$( cd "$( dirname "$0" )" &> /dev/null && pwd )
rm -fR runtime mill-standalone versions.properties out
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
$SCRIPT_HOME/mill-standalone all runtime.library.shared.tests.test runtime.library.jvm.tests.test runtime.library.js.tests.test
