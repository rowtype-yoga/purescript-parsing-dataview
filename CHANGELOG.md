# Changelog

## Unreleased

- New combinator: `anyTill`
- New parser: `anyCodePointUTF8`

## v3.1.0

Bugfix `takeN`.

## v3.0.0

### Breaking changes

- Build for PureScript v0.15.0 with __parsing__ v9.0.0.

## v2.1.0 2021-11-24

Licensing cleanup.

## v2.0.0 2021-07-21

Build for PureScript v0.14.0.

Added the `match` combinator.

### Breaking changes

Some typeclass constraints on the public API have changed due to changes
in the __arraybuffer__ package v11. For the most part we don’t expect
this will require any changes to code which depends on this package. If
you are using the `anyPrim` function you might have to change `AProxy` to
`Prelude.Proxy`. See for more details:

* https://github.com/purescript-contrib/purescript-arraybuffer/blob/main/CHANGELOG.md#v1100

