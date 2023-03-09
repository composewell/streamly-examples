# CAUTION! a spelling mistake in arg string is ignored silently.
#
# nix-shell --argstr c2nix "--flag examples-sdl"

# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import
      (builtins.fetchTarball
          https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz)
      {}
, compiler ? "ghc922"
, c2nix ? "" # cabal2nix CLI options
# TODO
#, sources ? [] # e.g. [./. ./benchmark]
#, hdeps ? [] # e.g. [time, mtl]
#, deps ? [] # e.g. [SDL2]
}:
let haskellPackages =
        if compiler == "default"
        then nixpkgs.haskellPackages
        else nixpkgs.haskell.packages.${compiler};

    # we can possibly avoid adding our package to HaskellPackages like
    # in the case of nix-shell for a single package?
    mkPackage = super: pkg: path: opts: inShell:
                let orig = super.callCabal2nixWithOptions pkg path opts {};
                 in if inShell
                    # Avoid copying the source directory to nix store by using
                    # src = null.
                    then orig.overrideAttrs (oldAttrs: { src = null; })
                    else orig;

    # sdl2 needs to be updated for ghc922
    # flags = "--flag fusion-plugin --flag sdl2 --flag interop" + " " + c2nix;
    flags = "--flag fusion-plugin --flag interop" + " " + c2nix;

    mkHaskellPackages = inShell:
        haskellPackages.override {
            # We could disbale doCheck on all like this, but it would make the
            # whole world rebuild, we can't use the binary cache
            #packageSetConfig = self: super: {
            #    mkDerivation = drv: super.mkDerivation (drv // {
            #        doCheck = false;
            #    });
            #};
            overrides = self: super:
                with nixpkgs.haskell.lib;
                {
                    streamly-examples =
                        mkPackage super "streamly-examples"
                            ./. flags inShell;

                    streamly =
                      nixpkgs.haskell.lib.overrideCabal
                        #(super.callHackageDirect
                        #  { pkg = "streamly";
                        #    ver = "0.9.0";
                        #    sha256 = "sha256-CjFq9SCdbgLZa7NqOE4OtC8OaFg4vK8VmIDjGU5rGko=";
                        #  } {})
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly.git";
                            rev = "147750cc437388b276e07f69c25ceb4d1229bcc7";
                        }; in super.callCabal2nix "streamly" src {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    streamly-core =
                      nixpkgs.haskell.lib.overrideCabal
                        (let src = fetchGit {
                            url = "git@github.com:composewell/streamly.git";
                            rev = "147750cc437388b276e07f69c25ceb4d1229bcc7";
                        }; in super.callCabal2nix "streamly-core" "${src}/core" {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    lockfree-queue =
                      super.callHackageDirect
                        { pkg = "lockfree-queue";
                          ver = "0.2.4";
                          sha256 = "1bj9agy3x0yjbscpjgn96gpnj4lvkh39spjvy3jnrr3a42v3ynw7";
                        } {};

                    #unicode-data =
                    #  super.callHackageDirect
                    #    { pkg = "unicode-data";
                    #      ver = "0.3.0";
                    #      sha256 = "sha256-3R8ZmLoN/oWU0Mr/V4o/90NqiWaE8fprVULgh8/s/Uc=";
                    #    } {};

                    fusion-plugin =
                      super.callHackageDirect
                        { pkg = "fusion-plugin";
                          ver = "0.2.5";
                          sha256 = "sha256-a5ZIi810Utsj0UsQZwnCaRYIJ8RWLUqppg4lYaNvOkM=";
                        } {};

                    # Example to Use a different version of a package
                    #QuickCheck = self.QuickCheck_2_14;

                    # Example to disable tests if tests fail or take too long
                    # or to use different configure flags if needed
                    #
                    # XXX We need the ability to disable doCheck on all
                    # those packages that are being built locally and
                    # not fetched from the cache. Running tests could do
                    # nasty things to the machine e.g. some tests even
                    # listen for incoming connections on the network.
                    #selective =
                    #    super.selective.overrideAttrs (oldAttrs:
                    #      { doCheck = false;
                    #        configureFlags =
                    #          oldAttrs.configureFlags ++ ["--disable-tests"];
                    #      });
                };
        };

    hspkgs = mkHaskellPackages true;

    # A fake package to add some additional deps to the shell env
    additionalDeps = hspkgs.mkDerivation rec {
              version = "0.1";
              pname   = "streamly-examples-additional";
              license = "BSD-3-Clause";

              libraryHaskellDepends = with hspkgs; [
                deque
              ];
              setupHaskellDepends = with hspkgs; [
                cabal-doctest
              ];
              executableFrameworkDepends = with hspkgs;
                # XXX On macOS cabal2nix does not seem to generate a
                # dependency on Cocoa framework.
                if builtins.currentSystem == "x86_64-darwin"
                then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                else [];
            };

    shell = hspkgs.shellFor {
        packages = p:
          [ p.streamly-examples
            additionalDeps
          ];
        shellHook = ''
          export CABAL_DIR="$(pwd)/.cabal.nix"
          if test -n "$PS_SHELL"
          then
            export PS1="$PS_SHELL\[$bldred\](nix)\[$txtrst\] "
          fi
        '';
    };
in if nixpkgs.lib.inNixShell
   then shell
   else (mkHaskellPackages false).streamly-examples
