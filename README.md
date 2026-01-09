## `graphql-tools` (Rust)

[Documentation](https://docs.rs/graphql-tools) | [Crate](https://crates.io/crates/graphql-tools) | [GitHub](https://github.com/dotansimha/graphql-tools-rs)

The [`graphql_tools` crate](https://crates.io/crates/graphql-tools) implements tooling around GraphQL for Rust libraries.

The goal of this library is to create a common layer of tools that has similar/improved APIs to [`graphql-js` reference implementation](https://github.com/graphql/graphql-js) and [`graphql-tools` from the JS/TS ecosystem](https://github.com/ardatan/graphql-tools).

### Getting Started

![Crates.io](https://img.shields.io/crates/v/graphql-tools?label=graphql-tools%20%28crates.io%29)

Add `graphql-tools` as a dependency of your project by adding the following to your `Cargo.toml` file:

```toml
[dependencies]
graphql-tools = "..."
```

Or, if you are using [`cargo-edit`](https://github.com/killercup/cargo-edit):

```
cargo add graphql-tools
```

#### Validation Rules

> This comparison is based on `graphql-js` reference implementation.

- [x] ExecutableDefinitions (not actually needed)
- [x] UniqueOperationNames
- [x] LoneAnonymousOperation
- [x] SingleFieldSubscriptions
- [x] KnownTypeNames
- [x] FragmentsOnCompositeTypes
- [x] VariablesAreInputTypes
- [x] LeafFieldSelections
- [x] FieldsOnCorrectType
- [x] UniqueFragmentNames
- [x] KnownFragmentNames
- [x] NoUnusedFragments
- [x] PossibleFragmentSpreads
- [x] NoFragmentCycles
- [x] UniqueVariableNames
- [x] NoUndefinedVariables
- [x] NoUnusedVariables
- [x] KnownDirectives
- [x] UniqueDirectivesPerLocation
- [x] KnownArgumentNames
- [x] UniqueArgumentNames
- [x] ValuesOfCorrectType
- [x] ProvidedRequiredArguments
- [x] VariablesInAllowedPosition
- [x] OverlappingFieldsCanBeMerged
- [ ] UniqueInputFieldNames (blocked by https://github.com/graphql-rust/graphql-parser/issues/59)
