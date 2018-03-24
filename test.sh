#!/bin/bash -e
rm -fR runtime mill-standalone versions.properties out
curl -Lo mill-standalone http://files.sireum.org/mill-standalone
chmod +x mill-standalone
curl -Lo versions.properties https://raw.githubusercontent.com/sireum/kekinian/master/versions.properties
./mill-standalone all runtime.library.shared.tests.test runtime.library.jvm.tests.test runtime.library.js.tests.test
