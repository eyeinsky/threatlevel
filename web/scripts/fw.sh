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
    local MSG='add default.nix'
    for pkgPath in $(local_deps) fw/web; do
        local P=$HOME/src/$pkgPath
        if [ -d "$P" ]; then
            (cd $HOME/src/$pkgPath && cabal2nix . > default.nix) 2>/dev/null
            echo_ $MSG: $pkgPath
        else
            echo_ $MSG: MISSING: $P
            exit 1
        fi
    done
}

# docker repl

repl-create() {
    docker pull nixos/nix
    docker tag nixos/nix fw-repl:stable
    fw repl
    docker commit docker-repl fw-repl:stable
}

nixos-repl() {
    add-default-nixs
    cabal2nix --shell . > shell.nix
    patch shell.nix local-deps.patch
    nix-shell --command 'cabal new-repl'
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
