{
  packageOverrides = pkgs:
  let
    lib = pkgs.haskell.lib;
  in {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        f = path: rest: lib.dontCheck (lib.dontHaddock (haskellPackagesNew.callPackage path rest));

        render = f /root/src/fw/render {};
        identifiers = f /root/src/fw/identifiers {};
        multiline = f /root/src/fw/multiline {};
        web-browser = f /root/src/fw/web-browser {};
        web-url = f /root/src/fw/web-url {};
        web = lib.justStaticExecutables (f ./default.nix {});
      };
    };
  };

  allowUnfree = true;
}
