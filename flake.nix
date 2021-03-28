{
  description = "A very basic flake";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
  }; 
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    with nixpkgs.lib;
    eachSystem ["x86_64-darwin"] (system:
      let version = "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay = self: super:
            with self;
            with haskell.lib;
            with haskellPackages;
            {
              monoid-extras = overrideCabal (callCabal2nix "monoid-extras" ./. {}) (o: { version = "${o.version}-${version}"; });
            };
          overlays = [ overlay ];
      in
        with (import nixpkgs { inherit system overlays; });
        {
          packages = flattenTree (recurseIntoAttrs { inherit monoid-extras; });
        });
}
