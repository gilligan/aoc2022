{ system ? builtins.currentSystem
, pins ? import ./npins
, pkgs ? import pins.nixpkgs { inherit system; }
, hooks ? import pins."pre-commit-hooks.nix"
}:

let
  pre-commit-check = hooks.run {
    src = ./.;
    tools = {
      ormolu = pkgs.ormolu;
      cabal-fmt = pkgs.haskellPackages.cabal-fmt;
      hlint = pkgs.hlint;
      nixpkgs-fmt = pkgs.nixpkgs-fmt;
    };
    hooks = {
      ormolu.enable = true;
      cabal-fmt.enable = true;
      hlint.enable = true;
      nixpkgs-fmt.enable = true;
    };
  };
  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${pkgs.ghcid}/bin/ghcid --clear \
    --command "cabal repl aoc2022:test:tests" \
    --test "hspec spec" \
    --setup "import Test.Hspec" \
    --restart=./src --restart=./test
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    niv
    nixpkgs-fmt
    ghc
    cabal-install
    ormolu
    hlint
    haskell-language-server
    pcre
    ghcid
    haskellPackages.cabal-fmt
    haskellPackages.haskell-ci
    watch-tests
  ];
  shellHook = ''
    ${pre-commit-check}
  '';
}
