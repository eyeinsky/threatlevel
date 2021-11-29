{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        rapid = f ../github-eyeinsky-rapid.nix {};
        fixedlist = f ../github-eyeinsky-fixedlist.nix {};
        render = f ../render {};
        identifiers = f ../identifiers {};
        multiline = f ../multiline {};
        threatlevel-browser = f ../threatlevel-browser {};
        threatlevel-url = f ../threatlevel-url {};
        threatlevel-common = f ../threatlevel-common {};
        threatlevel-js = f ../threatlevel-js {};
        threatlevel-css = f ../threatlevel-css {};
        web = lib.justStaticExecutables (f ./default.nix {});
      };
    };
  };

  allowUnfree = true;
}
