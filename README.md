# floreal-commons

## Use the prelude

In the cabal file of each component:

```cabal
  build-depends:
    base,
    floreal-prelude,

  mixins:
    base hiding (Prelude),
    floreal-prelude (Floreal.Commons.Prelude as Prelude),
```

Do not use `-Wunused-packages`. With these mixins, it reports `base` and `floreal-prelude` as unused.

## Versioning

The cabal version stays the same, but the releases will be tagged by the date,
in order to be more readable from the cabal project files.
