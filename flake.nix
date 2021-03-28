{
  description = "A very basic flake";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
  }; 
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    with nixpkgs.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay = self: super:
            with self;
            with haskell.lib;
            with haskellPackages;
            {
              monoid-extras = rec {
                package = overrideCabal (callCabal2nix "monoid-extras" ./. {}) (o: { version = "${o.version}-${version}"; });
                bench = mkApp { drv = package; exePath = "/bin/semi-direct-product-exe"; };
              };
            };
          overlays = [ overlay ];
      in
        with (import nixpkgs { inherit system overlays; });
        rec {
          packages = flattenTree (recurseIntoAttrs { monoid-extras = monoid-extras.package; });
          apps = { monoid-extras-bench = monoid-extras.bench; };
          defaultApp = apps.monoid-extras-bench;
        });
}
