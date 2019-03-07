import (builtins.fetchTarball {
  # This pinned version of nixpkgs contains Bazel 0.21. If you bump this, make
  # sure you update the name attribute to reflect the version you bumped to.
  # You can fetch a new `sha256` using `nix-prefetch-url --unpack <url>`.
  name = "nixos-unstable-2019-03-07";
  url = https://github.com/NixOS/nixpkgs/archive/6a69ce97506d5a3801418a8872fb73f52efe2b46.tar.gz;
  sha256 = "1vpwq0kavxfixh480dh1zzrjzshwwi5m3mg45r2aadb00ifbr1fi";
})
