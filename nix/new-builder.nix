{ stdenv, lib, haskellLib, ghc, hashes, fetchurl, writeText, runCommand, pkgconfig }:

{ flags ? {}
, package ? {}
, components ? {}

, name ? "${package.identifier.name}-${package.identifier.version}"
, sha256 ? hashes.${package.identifier.name}.${package.identifier.version}
, src ? fetchurl { url = "mirror://hackage/${name}.tar.gz"; inherit sha256; }

, doSublibs ? true
, doForeignLibs ? true
, doExecutables ? true
, doCheck ? false
, doBenchmarks ? false

, allowNewer ? true
, allowOlder ? false

, configureFlags ? []
}@config:

let
  foldEnabledComps = haskellLib.foldComponents (
    lib.optional doSublibs "sublibs"
    ++ lib.optional doForeignLibs "foreignlibs"
    ++ lib.optional doExecutables "exes"
    ++ lib.optional doCheck "tests"
    ++ lib.optional doBenchmarks "benchmarks"
  );

  concatFromEnabledComps = name:
    builtins.filter (p: p != null)
      (foldEnabledComps (_: comp: acc: acc ++ comp.${name} or []) [] config);

  deps = {
    depends =
      builtins.filter
        (p: p.identifier != package.identifier)
        (concatFromEnabledComps "depends");
    libs = concatFromEnabledComps "libs";
    frameworks = concatFromEnabledComps "frameworks";
    pkgconfig = concatFromEnabledComps "pkgconfig";
    buildTools = concatFromEnabledComps "build-tools";
  };

  defaultSetupSrc = builtins.toFile "Setup.hs" ''
    import Distribution.Simple
    main = defaultMain
  '';
  defaultSetup = runCommand "Setup" { nativeBuildInputs = [ghc]; } ''
    ghc ${defaultSetupSrc} --make -o $out
  '';

  flagsAndConfig = field: xs: {
    flags = map (x: "--${field}=${x}") xs;
    config = lib.optional (xs != []) "${field}: ${lib.concatStringsSep " " xs}";
  };

  allFlagsAndConfigs = {
    packageDbs =
      let
        makePairs = map (p: { key="${p}"; val=p; });
        closure = builtins.genericClosure {
          startSet = makePairs deps.depends;
          operator = {val,...}: makePairs val.deps.depends;
        };
        flatDepends = map ({val,...}: val) closure;
      in flagsAndConfig "package-db" (map (p: "${p}/package.conf.d") flatDepends);

    extraLibDirs = flagsAndConfig "extra-lib-dirs" (map (p: "${p}/lib") deps.libs);
    extraIncludeDirs = flagsAndConfig "extra-include-dirs" (map (p: "${lib.getDev p}/include") deps.libs);
    extraFameworks = flagsAndConfig "extra-framework-dirs" (map (p: "${p}/Library/Frameworks") deps.frameworks);
    allowNewer = lib.mapAttrs (_: lib.optional allowNewer) { flags = "--allow-newer"; config = "allow-newer: True"; };
    allowOlder = lib.mapAttrs (_: lib.optional allowOlder) { flags = "--allow-older"; config = "allow-older: True"; };
    userFlags = {
      flags = [("--flags=\"" + lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: lib.optionalString (!val) "-" + fname) flags) + "\"")];
      config = [];
    };
  };

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=$out" ]
    ++ builtins.concatLists (lib.mapAttrsToList (_: x: x.flags) allFlagsAndConfigs)
    ++ configureFlags
  );

in stdenv.mkDerivation {
  inherit name src;
  passthru = {
    inherit deps;
    inherit (package) identifier;
  };

  enableParallelBuilding = true;

  meta = {
    homepage = package.homepage;
    description = package.synopsis;
    license = (import ./cabal-licenses.nix stdenv).${package.license};
  };

  # Environment
  CABAL_CONFIG = writeText
    "package-db-cabal.config"
    (lib.concatStringsSep "\n" (builtins.concatLists (lib.mapAttrsToList (_: x: x.config) allFlagsAndConfigs)));

  # Deps
  nativeBuildInputs =
    [ghc]
    ++ deps.pkgconfig
    ++ lib.optional (deps.pkgconfig != []) pkgconfig
    ++ deps.buildTools;

  # Phases
  configurePhase = ''
    setuphs=${defaultSetup}
    for f in Setup.hs Setup.lhs; do
      if [ -f $f ]; then
        if (diff $f ${defaultSetupSrc} > /dev/null); then
          setuphs=${defaultSetup}
        else
          ghc $f --make -o ./Setup
          setuphs=$(pwd)/Setup
        fi
        break
      fi
    done
    echo Configure flags:
    printf "%q " ${finalConfigureFlags}
    echo
    $setuphs configure ${finalConfigureFlags}
  '';

  buildPhase = ''
    $setuphs build -j$NIX_BUILD_CORES
  '';

  checkPhase = lib.optionalString doCheck ''
    $setuphs test -j$NIX_BUILD_CORES
  '';

  installPhase = ''
    $setuphs copy
    ${lib.optionalString (haskellLib.hasLibrary config) ''
      $setuphs register --gen-pkg-config=${name}.conf
      ghc-pkg init $out/package.conf.d
      ghc-pkg ${lib.concatStringsSep " " allFlagsAndConfigs.packageDbs.flags} -f $out/package.conf.d register ${name}.conf
    ''}
  '';
}
