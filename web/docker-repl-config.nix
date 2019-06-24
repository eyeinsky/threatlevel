{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        rapid = f /root/src/rapid {};
        ds = f /root/src/ds {};
        hs-minu = f /root/src/hs-minu {};
        render = f /root/src/fw/render {};
        identifiers = f /root/src/identifiers {};
        web-browser = f /root/src/fw/web-browser {};
        web-url = f /root/src/fw/web-url {};
        web = lib.justStaticExecutables (f ./default.nix {});
      };
    };
  };

  allowUnfree = true;
}
