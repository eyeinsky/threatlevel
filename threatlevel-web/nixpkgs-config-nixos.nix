{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        rapid = f ../github-eyeinsky-rapid.nix {};
        render = f ../render {};
        identifiers = f ../identifiers {};
        multiline = f ../multiline {};
        web-browser = f ../web-browser {};
        web-url = f ../web-url {};
        web-common = f ../web-common {};
        web-js = f ../web-js {};
        web-css = f ../web-css {};
        web = lib.justStaticExecutables (f ./default.nix {});
      };
    };
  };

  allowUnfree = true;
}
