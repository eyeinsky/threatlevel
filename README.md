# threatlevel

## Develop

```bash
stack repl
```

### Cabal

Cabal works best with nix below, otherwise you'll have to manually
make sure to have non-haskell dependencies installed.

### Nix

```bash
nix-shell
cabal repl
```

The default.nix adds required dependencies.
