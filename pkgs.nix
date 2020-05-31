pkgs: with pkgs;
[
  (callPackage ./nix/language-tl.nix {})
  prettyprinter
  megaparsec
  lens
  generic-lens
  aeson
  base64-bytestring-type
  pretty-simple
  th-lift-instances
]
