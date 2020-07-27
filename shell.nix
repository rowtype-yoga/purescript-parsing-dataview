{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0ba91d9aa9f7421f6bfe4895677159a8a999bf20";
      sha256 = "1baq7mmd3vjas87f0gzlq83n2l1h3dlqajjqr7fgaazpa9xgzs7q";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_13_8
    easy-ps.spago
    pkgs.nodejs-13_x
    pkgs.nodePackages.bower
    # pkgs.nodePackages.pulp
    #
    # The pulp nix derivation doesn't work, so to run pulp:
    # $ npm install pulp
    # $ node node_modules/pulp/index.js
  ];
  LC_ALL = "C.UTF-8"; # https://github.com/purescript/spago/issues/507
}
