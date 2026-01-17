#!/bin/bash

tag=hs-fs-filter:0.1.0.0
wtg=hs-fs-filter-wasi:0.1.0.0

build_apple() {
	container image inspect "${tag}" |
		jq --raw-output '.[].name' |
		fgrep -q "$tag" &&
		return

	echo building image "${tag}"...

	container \
		build \
		--file ./Dockerfile \
		--platform linux/arm64 \
		--progress plain \
		--tag "${tag}" \
		.
}

build_apple_wasi() {
	container image inspect "${wtg}" |
		jq --raw-output '.[].name' |
		fgrep -q "$wtg" &&
		return

	echo building image "${wtg}"...

	container \
		build \
		--file ./Dockerfile.wasi.txt \
		--platform linux/arm64 \
		--progress plain \
		--tag "${wtg}" \
		.
}

#build_apple
build_apple_wasi
