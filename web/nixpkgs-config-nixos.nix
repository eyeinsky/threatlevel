{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        rapid = f ../github-eyeinsky-rapid.nix {};
        render = f ~/src/fw/render {};
        identifiers = f ~/src/fw/identifiers {};
        multiline = f ~/src/fw/multiline {};
        web-browser = f ~/src/fw/web-browser {};
        web-url = f ~/src/fw/web-url {};
        web = lib.justStaticExecutables (f ./default.nix {});
      };
    };
  };

  allowUnfree = true;
}
