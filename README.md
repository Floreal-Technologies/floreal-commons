# floreal-commons

These are the commonly-used helpers, types and re-exports for the floreal.tech
Haskell projects.

They are rather unsuitable for outside usage, will not care about backwards
compatibility, and should be taken as inspiration instead of being re-used.

## Use the prelude

In cabal.project:

```cabal
source-repository-package
  type: git
  location: https://github.com/Floreal-Technologies/floreal-commons
  tag: 2026-10-19
  subdir:
    ./floreal-prelude
    ./floreal-commons
```

In the cabal file of each component:

```cabal
  build-depends:
    base,
    floreal-prelude,

  mixins:
    base hiding (Prelude),
    floreal-prelude (Floreal.Commons.Prelude as Prelude),
```

Do not use `-Wunused-packages`. With these mixins, it reports `base` and
`floreal-prelude` as unused.

## Versioning

The cabal version stays the same, but the releases will be tagged by the date,
in order to be more readable from the cabal project files.
