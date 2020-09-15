#!/usr/bin/env bash

REPO_PATH="$(cd $(dirname $0)/.. && pwd)"

LOCAL_DEPS=$(cat <<END
- identifiers
- multiline
- render
- web-browser
- web-url
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
    for PKG_PATH in $(local_deps) web; do
        echo "In '$PKG_PATH' run '$DO_WHAT':"
        (
            cd $PKG_PATH
            eval $DO_WHAT
        )
    done
}

prepare() {
    in_deps_do "cabal2nix . > default.nix"
    (
        cd web
        cabal2nix --shell . > shell.nix
        patch shell.nix local-deps.patch
    )
}

repl() {
    prepare
    cd web
    CMD="${@:-cabal repl}"
    nix-shell --command "$CMD"
}



set -e
cd $REPO_PATH
$@
