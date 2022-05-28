#!/usr/bin/env bash

REPO_PATH="$(cd $(dirname $0)/.. && pwd)"

LOCAL_DEPS=$(cat <<END
- identifiers
- render
- threatlevel-common
- threatlevel-css
- threatlevel-js
- threatlevel-url
- threatlevel-web
- threatlevel-unsorted
END
);

local_deps() {
    echo "$LOCAL_DEPS" | yq -r '.[]'
}

# library

echo_() {
    echo fw: $@
}

in_deps_do() {
    local DO_WHAT=$@
    for PKG_PATH in $(local_deps); do
        echo "In '$PKG_PATH' run '$DO_WHAT':"
        (
            cd $PKG_PATH
            eval $DO_WHAT
        )
    done
}

dev_init() {
    # Use forked rapid (has updated dependency bounds)
    cabal2nix https://github.com/eyeinsky/fixedlist.git > github-eyeinsky-fixedlist.nix
}

prepare() {
    local PKG="${1:-threatlevel-web}"
    in_deps_do "hpack && cabal2nix . > default.nix"
    cabal2nix --shell "$PKG" > shell.nix
    patch shell.nix nix/local-deps.patch
}

repl() {
    prepare
    CMD="${@:-cabal repl}"
    nix-shell --command "$CMD"
}



set -e
cd $REPO_PATH
$@
