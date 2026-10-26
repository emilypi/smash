# smash-aeson: Aeson support for the smash library


[![CI](https://github.com/emilypi/smash/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/emilypi/smash/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/smash-aeson.svg)](https://hackage.haskell.org/package/smash-aeson)

The instances use an explicit, stable object format across supported `aeson`
versions. Import the corresponding `Data.Can.Aeson`, `Data.Smash.Aeson`, or
`Data.Wedge.Aeson` module to bring the instances into scope.

| Value | JSON |
| --- | --- |
| `Non`, `Nada`, `Nowhere` | `{}` |
| `One a` | `{"One": a}` |
| `Eno b` | `{"Eno": b}` |
| `Two a b` | `{"One": a, "Eno": b}` |
| `Smash a b` | `{"SmashL": a, "SmashR": b}` |
| `Here a` | `{"Here": a}` |
| `There b` | `{"There": b}` |

Ordinary and lifted instances use the same representation. Object key order is
irrelevant when decoding. Unexpected or missing keys are rejected, and nullable
payloads retain their constructor keys even when their values are `null`.
