let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  nixpkgs = lock.nodes.nixpkgs.locked;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/${nixpkgs.owner}/${nixpkgs.repo}/archive/${nixpkgs.rev}.tar.gz";
    sha256 = nixpkgs.narHash;
  }) { };
in

pkgs.mkShell {
  packages = [
    pkgs.go_1_26
    pkgs.nodejs_22
    pkgs.ripgrep
  ];

  shellHook = ''
    echo "Entered nix-shell"
    echo "go:   $(go version)"
    echo "node: $(node --version)"
    echo "rg:   $(rg --version | head -n 1)"
  '';
}
