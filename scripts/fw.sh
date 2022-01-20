#!/usr/bin/env bash

REPO_PATH="$(cd $(dirname $0)/.. && pwd)"

LOCAL_DEPS=$(cat <<END
- identifiers
- multiline
- render
- threatlevel-common
- threatlevel-css
- threatlevel-js
- threatlevel-url
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
    for PKG_PATH in $(local_deps) threatlevel-web; do
        echo "In '$PKG_PATH' run '$DO_WHAT':"
        (
            cd $PKG_PATH
            eval $DO_WHAT
        )
    done
}

dev_init() {
    # Use forked rapid (has updated dependency bounds)
    cabal2nix https://github.com/eyeinsky/rapid.git > github-eyeinsky-rapid.nix
    cabal2nix https://github.com/eyeinsky/fixedlist.git > github-eyeinsky-fixedlist.nix
}

prepare() {
    in_deps_do "cabal2nix . > default.nix"
    (
        cd threatlevel-web
        cabal2nix --shell . > shell.nix
        patch shell.nix local-deps.patch
    )
}

repl() {
    prepare
    cd threatlevel-web
    CMD="${@:-cabal repl}"
    nix-shell --command "$CMD"
}



set -e
cd $REPO_PATH
$@
