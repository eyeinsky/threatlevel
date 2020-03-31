#!/usr/bin/env bash

REPO_PATH="$(dirname $0)/.."

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

set -e

# library

echo_() {
    echo fw: $@
}

add-default-nixs() {
    local MSG='add default.nix'
    for PKG_PATH in $(local_deps) web; do
        local P=$REPO_PATH/$PKG_PATH
        if [ -d "$P" ]; then
            (cd $P && hpack && cabal2nix . > default.nix)
            echo_ $MSG: $PKG_PATH
        else
            echo_ $MSG: MISSING: $P
            exit 1
        fi
    done
}

repl() {
    CMD="${@:-cabal new-repl}"
    add-default-nixs
    cd web
    cabal2nix --shell . > shell.nix
    patch shell.nix local-deps.patch
    nix-shell --command "$CMD"
}

cd $REPO_PATH
$@
