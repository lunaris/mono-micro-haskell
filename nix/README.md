# Nix

## What we use Nix for

We rely on [Nix](https://nixos.org/nix/), the purely functional package manager,
for two things:

  1. To provision development environments with a common set of tools, pinned
     to concrete versions. The aim here is to minimise scenarios where what
     worked on Alice's box breaks on Bob's when he pulls her changes due to him
     having the "wrong" version of something. This is done in the
     [shell.nix](shell.nix) file, where we specify as `buildInputs` things such
     as `bazel`, `docker` or `git`.

  2. To provision, in our Bazel rules, things like `ghc`, Haskell libraries
     from Hackage (including their C dependencies), etc.

## Pinning nixpkgs

To ensure everybody runs the same version of Nix, with the same database of
packages, we pin down the version to a concrete Git commit in
[nix/nixpkgs/default.nix](./nixpkgs/default.nix).

## Haskell dependecies

In [nix/haskell/haskell-packageset](./haskell/haskell-packageset/default.nix) we
pin down the version of GHC and the versions of all external packages we use.
