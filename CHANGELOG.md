# 0.3.10

- Support `aeson-2.0.0.0`

# 0.3.9

- Add `ToJSON/FromJSON Void`

# 0.3.8

- Re-implement `withNumber`
- Add `withEmbeddedJSON`

# 0.3.7

- Use `attoparsec-iso8601` time parsers.
- Don't export `GToJSON` etc. members.

# 0.3.6

- Fix accidental removal of `AesonException` export

# 0.3.5.2

- Support QuickCheck-2.9.1

# 0.3.5.1

- Fix `(.=)` export regression introduced by 0.3.5.0

# 0.3.5.0

- Use explicit export list. Now we are sure we don't break interface.
- `value`, `value'` and `Parser` are exported from `Data.Aeson.Compat`

# 0.3.4.0

- Add `NominalDiffTime` instances

# 0.3.3.0

- Enable `PolyKinds` to generalize `Proxy`, `Tagged`, and `Const` instances.

# 0.3.2.0

- Introduce instances from `aeson-0.11.1.0`: `Const`, `Tagged`, `Proxy` and `NonEmpty`
- Fix bug with `Natural` instance, `aeson-0.11.1.0` and `base <=4.7`

# 0.3.1.0

- `aeson-0.11` support
- GHC 8.0.1 support
- Add `ToJSON` `Day` and `LocalTime` instances
  - *NOTE* this instances are broken in `aeson-0.10.0.0`
- Add `Natural`, `Ordering` and `Version` instances

# 0.3.0.0

Split out `aeson-extra`
