with import <nixpkgs> {};

let
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocamlPackages_latest;
in

pkgs.mkShell {
  name = "99-problems";
  buildInputs = with pkgs; [
  ] ++ ( with ocamlPackages; [
    ocaml
    findlib
    utop
  ]);
}
