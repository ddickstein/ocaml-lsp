# paramaterised derivation with dependencies injected (callPackage style)
{ pkgs, stdenv, fetchFromGitHub, opam2nix }:
let
  strings = pkgs.lib.strings;
  ourSrc = let ignores = pkgs.lib.strings.fileContents ../.gitignore
             + builtins.foldl' (acc: e: acc + "\n" + e) "\n"
                 [ "nix" "shell.nix" "default.nix" ];
           in pkgs.nix-gitignore.gitignoreSourcePure ignores ../.;
  args = {
    inherit (pkgs.ocaml-ng.ocamlPackages_4_14) ocaml;
    selection = ./opam-selection.nix;
    src = {
      lsp = ourSrc;
      jsonrpc = ourSrc;
      ocaml-lsp-server = ourSrc;
    };
  };
  opam-selection = opam2nix.build args;
  localPackages = let contents = builtins.attrNames (builtins.readDir ../.);
  in builtins.filter (strings.hasSuffix ".opam") contents;
  resolve = opam2nix.resolve args (localPackages ++ [
    # dev deps
    "cinaps"
    "menhir"
    "ppx_yojson_conv"

    # test deps
    "ppx_expect"
    "ocamlformat-rpc"
  ]);

in (builtins.listToAttrs (builtins.map (fname:
  let packageName = strings.removeSuffix ".opam" fname;
  in {
    name = packageName;
    value = builtins.getAttr packageName opam-selection;
  }) localPackages)) // {
    inherit resolve;
    opam = opam-selection;
  }
