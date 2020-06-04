pkgs: with pkgs;
let
  language-tl = callPackage ./nix/language-tl.nix { };
in
[
  language-tl
  prettyprinter
  megaparsec
  lens
  generic-lens
  aeson
  base64-bytestring-type
  pretty-simple
]
