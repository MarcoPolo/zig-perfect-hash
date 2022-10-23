{
  description = "zig-multibase";
  inputs.nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.zig-overlay.url = "github:mitchellh/zig-overlay";

  outputs = { self, nixpkgs, nixpkgs-darwin, flake-utils, zig-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import (if system == "aarch64-darwin" then nixpkgs-darwin else nixpkgs) { system = system; };
        zig = zig-overlay.packages.${system}."master-2022-10-06";
        zig9 = zig-overlay.packages.${system}."0.9.0";
      in
      {

        packages.zls = pkgs.stdenvNoCC.mkDerivation {
          name = "zls";
          version = "master";
          src = pkgs.fetchFromGitHub {
            owner = "zigtools";
            repo = "zls";
            rev = "0.9.0";
            fetchSubmodules = true;
            sha256 = "sha256-MVo21qNCZop/HXBqrPcosGbRY+W69KNCc1DfnH47GsI=";
            # sha256 = pkgs.lib.fakeSha256;
          };
          nativeBuildInputs = [
            zig9
            # pkgs.autoPatchelfHook # Automatically setup the loader, and do the magic
          ];
          dontConfigure = true;
          dontInstall = true;
          buildPhase = ''
            mkdir -p $out
            zig build install -Drelease-safe=true -Ddata_version=master --prefix $out
          '';
          XDG_CACHE_HOME = ".cache";
        };
        packages.hello = pkgs.hello;
        defaultPackage = self.packages.${system}.hello;
        devShell = pkgs.mkShell {
          buildInputs = [ pkgs.hello zig self.packages.${system}.zls ];
        };
      });
}
