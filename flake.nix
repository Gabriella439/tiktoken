{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.05";

    utils.url = "github:numtide/flake-utils/v1.0.0";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          tiktoken =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.tiktoken;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              base64 = "1.0";

              tiktoken = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskellPackages.tiktoken;

          apps.default = {
            type = "app";

            program = "${pkgs.tiktoken}/bin/tiktoken";
          };

          devShells.default = pkgs.haskellPackages.tiktoken.env;
        }
    );
}
