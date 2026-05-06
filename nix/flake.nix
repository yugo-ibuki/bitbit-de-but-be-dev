{
  description = "Small Nix sample for package management, dev shells, and runnable apps";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forAllSystems =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          f {
            inherit system;
            pkgs = import nixpkgs { inherit system; };
          }
        );
    in
    {
      packages = forAllSystems (
        { pkgs, ... }:
        {
          default = pkgs.writeShellApplication {
            name = "nix-node-sample";
            runtimeInputs = [
              pkgs.nodejs_22
            ];
            text = ''
              exec node ${./hello.js} "$@"
            '';
          };
        }
      );

      apps = forAllSystems (
        { system, ... }:
        {
          default = {
            type = "app";
            program = "${self.packages.${system}.default}/bin/nix-node-sample";
          };
        }
      );

      devShells = forAllSystems (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            packages = [
              pkgs.go_1_26
              pkgs.nodejs_22
              pkgs.ripgrep
            ];

            shellHook = ''
              echo "Entered nix devShell"
              echo "go:   $(go version)"
              echo "node: $(node --version)"
              echo "rg:   $(rg --version | head -n 1)"
            '';
          };
        }
      );
    };
}
