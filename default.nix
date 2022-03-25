# CAUTION! a spelling mistake in arg string is ignored silently.
#
# nix-shell --argstr c2nix "--flag examples-sdl"

# To use ghc-8.6.5
# nix-shell --argstr compiler "ghc865"

{
  nixpkgs ?
    import
      (builtins.fetchTarball
          https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz)
      {}
, compiler ? "default"
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

    flags = "--flag fusion-plugin --flag sdl2 --flag interop" + " " + c2nix;

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
                        (super.callHackageDirect
                          { pkg = "streamly";
                            ver = "0.8.2";
                            sha256 = "0jhsdd71kqw0k0aszg1qb1l0wbxl1r73hsmkdgch4vlx43snlc8a";
                          } {})
                        (old:
                          { librarySystemDepends =
                              if builtins.currentSystem == "x86_64-darwin"
                              then [nixpkgs.darwin.apple_sdk.frameworks.Cocoa]
                              else [];
                            enableLibraryProfiling = false;
                            doHaddock = false;
                          });

                    unicode-data =
                      super.callHackageDirect
                        { pkg = "unicode-data";
                          ver = "0.3.0";
                          sha256 = "0izxxk7qgq22ammzmwc4cs4nlhzp7y55gzyas2a8bzhdpac1j7yx";
                        } {};

                    fusion-plugin =
                      super.callHackageDirect
                        { pkg = "fusion-plugin";
                          ver = "0.2.3";
                          sha256 = "073wbhdxj1sh5160blaihbzkkhabs8s71pqhag16lvmgbb7a3hla";
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
