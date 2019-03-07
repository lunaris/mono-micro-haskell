workspace(name = "mono")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")


# {{ NIXPKGS ###################################################################
# We use Nix to provision external dependencies (e.g., compilers) and ensure
# that exactly the same environment is used on every build.

http_archive(
  name = "io_tweag_rules_nixpkgs",
  sha256 = "5a384daa57b49abf9f0b672852f1a66a3c52aecf9d4d2ac64f6de0fd307690c8",
  strip_prefix = "rules_nixpkgs-0.5.2",
  urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.5.2.tar.gz"],
)

load(
  "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
  "nixpkgs_local_repository",
  "nixpkgs_package"
)

# nixpkgs is the official repository of, ahem, packages packaged with Nix. It
# is just a git repository on Github. We want to pin the version of nixpkgs
# (the commit at the head of the repo), so we make sure that we are all
# installing precisely the same version of everything we provision. The pinned
# version of nixpkgs is defined in `nix/nixpkgs/default.nix`, so that we can
# also use it from `shell.nix` (which we use to provision the developer
# environment).

nixpkgs_local_repository(
  name = "nixpkgs",
  nix_file = "//nix/nixpkgs:default.nix",
)

# }} NIXPKGS ###################################################################

# {{ HASKELL ###################################################################

http_archive(
  name = "io_tweag_rules_haskell",
  sha256 = "7834af7c33ee545763e703ed005bc40ae5cb2986fc64988a4f30ab17b7340ccb",
  strip_prefix = "rules_haskell-1fc0d17acae59677089e7ab3cae3af14b50ba297",
  # Pinned to our fork until this MR https://github.com/tweag/rules_haskell/pull/695 is resolved
  urls = ["https://github.com/heyhabito/rules_haskell/archive/1fc0d17acae59677089e7ab3cae3af14b50ba297.tar.gz"],
)

# Import external repositories that `rules_haskell` needs to work properly.

load(
  "@io_tweag_rules_haskell//haskell:repositories.bzl",
  "haskell_repositories",
)

haskell_repositories()

# Provision a GHC compiler using Nix. It will be available as the external
# repository @ghc_nixpkgs//...

nixpkgs_package(
  name = "ghc_nixpkgs",
  repositories = {"nixpkgs": "@nixpkgs//:default.nix"},
  attribute_path = "haskell.compiler.ghc863",
)

# Provision HLint using Nix. It will be available as the external repository
# @hlint_nixpkgs//...

nixpkgs_package(
  name = "hlint_nixpkgs",
  repositories = {"nixpkgs": "@nixpkgs//:default.nix"},
  attribute_path = "hlint",
)

# `//:ghc` is a `haskell_toolchain` defined in the `BUILD.bazel` file. The
# toolchain is configured to use the compiler under `@ghc_nixpkgs`. We register
# it here so all haskell rules can use it.

register_toolchains("//:ghc")

# Make external haskell libraries available. This works in two stages: first we
# use `haskell_nixpkgs_packageset()` to turn a Nix expression providing,
# essentially, all of Hackage (each pinned to a fixed version) into a Bazel
# macro `import_packages()` that, when run, will run a repository rule per
# Haskell library. Because repository rules are run lazily, in practice,
# Bazel will only end up installing those Haskell packages that we actually
# depend on.

load(
  "@io_tweag_rules_haskell//haskell:nixpkgs.bzl",
  "haskell_nixpkgs_packageset",
  "haskell_nixpkgs_packages",
)

haskell_nixpkgs_packageset(
  name = "haskell_packageset",
  nix_file = "//nix/haskell:haskell-packageset/default.nix",
  nix_file_deps = [ "//nix/nixpkgs:default.nix" ],
  base_attribute_path = "haskellPackages",
)

load(
  "@haskell_packageset//:packages.bzl",
  "import_packages",
)

import_packages(
  name = "hackage",
)

# }} HASKELL ###################################################################

# {{ DOCKER ####################################################################

http_archive(
  name = "io_bazel_rules_docker",
  sha256 = "29d109605e0d6f9c892584f07275b8c9260803bf0c6fcb7de2623b2bedc910bd",
  strip_prefix = "rules_docker-0.5.1",
  urls = ["https://github.com/bazelbuild/rules_docker/archive/v0.5.1.tar.gz"],
)

# Install those repositories that are needed by the container rules.

load(
  "@io_bazel_rules_docker//container:container.bzl",
  "container_pull",
  container_repositories = "repositories",
)

container_repositories()

# Downloads `library/debian:stretch-slim` from Docker Hub, and makes it
# available as the `@debian//image` target.

container_pull(
  name = "debian",
  registry = "index.docker.io",
  repository = "library/debian",
  tag = "stretch-slim",
)

# }} DOCKER ####################################################################
