{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system; overlays =
          [
            (pself: psuper: {
              ocamlPackages = psuper.ocamlPackages.overrideScope (oself: osuper: {
                cmdliner = osuper.cmdliner.overrideAttrs (a:
                  let version = "1.3.0"; in
                  {
                    inherit version;
                    src = builtins.fetchurl {
                      url = "https://erratique.ch/software/${a.pname}/releases/${a.pname}-${version}.tbz";
                      sha256 = "1fwc2rj6xfyihhkx4cn7zs227a74rardl262m2kzch5lfgsq10cf";
                    };
                  });
              });
            })
          ];
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "aoc2015";
          version = "n/a";
          src = ./.;
          buildInputs = with pkgs.ocamlPackages; [ angstrom cmdliner digestif hex_encode ocamlgraph ppx_jane yojson ];
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocamlformat_0_26_1
          ];
        };
      });
}
