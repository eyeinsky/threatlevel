#!/usr/bin/env bash

REPO_PATH=~/src/fw/web

LOCAL_DEPS=$(cat <<END
- fw/identifiers
- fw/multiline
- fw/render
- fw/web-browser
- fw/web-url
END
);

local_deps() {
    echo "$LOCAL_DEPS" | yq -r '.[]'
}


set -e

# library

echo_() {
    echo fw: $@
}

docker-local-env() { eval $(docker-machine env -u); }

add-default-nixs() {
    for pkgPath in $(local_deps) fw/web; do
        (cd $HOME/src/$pkgPath && cabal2nix . > default.nix) 2>/dev/null
        echo $pkgPath
    done
}

# docker repl

repl-create() {
    docker pull nixos/nix
    docker tag nixos/nix fw-repl:stable
    fw repl
    docker commit docker-repl fw-repl:stable
}

repl() {
    add-default-nixs
    hpack
    cabal2nix --shell . > shell.nix
    patch -i shell.nix.patch
    docker run -it --rm \
           -p 3023:22 \
           -p 8444:8443 \
           -p 80:80 \
           -p 443:443 \
           -v $HOME/src:/root/src \
           -v $HOME/.ssh:/root/.ssh \
           -w /root/src/fw/web \
           -e NIX_PATH='nixpkgs=/root/src/nixpkgs:/nix/var/nix/profiles/per-user/root/channels' \
           --name docker-repl \
           fw-repl:stable nix-shell
}

# builder

builder-create() {
    docker run -d \
           --restart always \
           -p 3022:22 \
           --name nix-docker \
           fw-builder:stable

}

builder-start() {
    docker start nix-docker
}

builder-stop() {
    docker stop nix-docker
}

cd $REPO_PATH
$@
