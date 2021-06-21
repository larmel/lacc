#!/bin/sh

distro="$1"
if [ -z "$distro" ] || [ ! -f test/docker/${distro}.Dockerfile ]
then
	echo "No Dockerfile found for '${distro}'."
	exit 1
fi

mkdir -p bin/docker/${distro}
cp test/docker/${distro}.Dockerfile bin/docker/${distro}/Dockerfile
cp .gitignore bin/docker/${distro}/Dockerfile.dockerignore

sudo DOCKER_BUILDKIT=1 docker build . -f bin/docker/${distro}/Dockerfile -t lacc_${distro}

sudo docker run -it --rm lacc_${distro}
