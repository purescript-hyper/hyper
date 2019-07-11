let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/packages.dhall sha256:9905f07c9c3bd62fb3205e2108515811a89d55cff24f4341652f61ddacfcf148

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
