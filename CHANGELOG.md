# Changelog

## v2.0.0 2021-07-21

Build for PureScript v0.14.

Added the `match` combinator.

### Breaking changes

Some typeclass constraints on the public API have changed due to changes
in the __arraybuffer__ package v11. For the most part we don’t expect
this will require any changes to code which depends on this package. If
you are using the `anyPrim` function you might have to change `AProxy` to
`Prelude.Proxy`. See for more details: 

* https://github.com/purescript-contrib/purescript-arraybuffer/blob/main/CHANGELOG.md#v1100

