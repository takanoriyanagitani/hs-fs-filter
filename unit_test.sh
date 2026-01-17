#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

echo "Running unit tests with Cabal..."

# Run the tests. Cabal will handle compilation and execution.
cabal test

echo "All unit tests PASSED!"
