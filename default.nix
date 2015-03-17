{ cabal ? (import <nixpkgs> {}).pkgs.skedgeDeps.cabal
, deepseq ? (import <nixpkgs> {}).pkgs.skedgeDeps.deepseq
, transformers ? (import <nixpkgs> {}).pkgs.skedgeDeps.transformers
}:

cabal.mkDerivation (self: {
  pname = "indexed-map";
  version = "0.1";
  src = /home/danharaj/code/skedge/indexed-map;
  buildDepends = [ deepseq transformers ];
  meta = {
    description = "Indexed map and set";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
