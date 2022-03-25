{ nixpkgs ? import <nixpkgs> { config = import ./nixpkgs-config-nixos.nix; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, base64-bytestring
      , blaze-builder, boomerang, bytestring, case-insensitive
      , containers, cookie, cryptonite, data-default, directory, dlist
      , file-embed, hashable, hpack, http-client, http-types, identifiers
      , lens, lens-aeson, lib, memory, mime-types, mtl, multiline
      , mwc-random, network, process, rapid, render, tagged
      , template-haskell, text, text-format, threatlevel-browser
      , threatlevel-common, threatlevel-css, threatlevel-js
      , threatlevel-url, time, transformers, unordered-containers, vector
      , wai, wai-extra, wai-websockets, warp, warp-tls, websockets, wreq
      }:
      mkDerivation {
        pname = "threatlevel-web";
        version = "0.2.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson async base base64-bytestring blaze-builder boomerang
          bytestring case-insensitive containers cookie cryptonite
          data-default directory dlist file-embed hashable http-client
          http-types identifiers lens lens-aeson memory mime-types mtl
          multiline mwc-random network process rapid render tagged
          template-haskell text text-format threatlevel-browser
          threatlevel-common threatlevel-css threatlevel-js threatlevel-url
          time transformers unordered-containers vector wai wai-extra
          wai-websockets warp warp-tls websockets wreq
        ];
        libraryToolDepends = [ hpack ];
        testHaskellDepends = [
          aeson async base base64-bytestring blaze-builder boomerang
          bytestring case-insensitive containers cookie cryptonite
          data-default directory dlist file-embed hashable http-client
          http-types identifiers lens lens-aeson memory mime-types mtl
          multiline mwc-random network process rapid render tagged
          template-haskell text text-format threatlevel-browser
          threatlevel-common threatlevel-css threatlevel-js threatlevel-url
          time transformers unordered-containers vector wai wai-extra
          wai-websockets warp warp-tls websockets wreq
        ];
        prePatch = "hpack";
        license = lib.licenses.gpl2Only;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
