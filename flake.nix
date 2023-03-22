{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [rust-overlay.overlays.default];
      };
    toolchain = pkgs.rust-bin.fromRustupToolchainFile ./toolchain.toml;
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [
        toolchain
        pkgs.babashka
        pkgs.clojure
        pkgs.graalvm-ce
        pkgs.llvmPackages.libclang
        pkgs.rust-analyzer-unwrapped
      ];
      RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
      LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
    };
  };
}
