<!-- cargo-rdme start -->

Lightweight declarative macro for sets of strings.

```rust
strum_lite::strum! {
    pub enum Casing {
        Kebab = "kebab-case",
        ScreamingSnake = "SCREAMING_SNAKE",
    }
}
```

# Features
- Implements [`FromStr`](core::str::FromStr) and [`Display`](core::fmt::Display).
- Attributes (docs, `#[derive(..)]`s) are passed through to the definition and variants.
- Aliases are supported.
- Custom enum discriminants are passed through.
- Generated code is `#![no_std]`.
- The generated [`FromStr::Err`](core::str::FromStr) provides a helpful error message.
- You may ask for a custom error type rather than using this crate's [`ParseError`].

```rust
strum_lite::with_error! {
    #[derive(Default)]
    #[repr(u8)]
    pub enum Casing {
        Kebab = "kebab-case" | "kebab" = 1,
        #[default]
        ScreamingSnake = "SCREAMING_SNAKE" = 1 + 2,
    }
    throws
    #[derive(Clone, Copy)]
    ParseCasingError;
}
```

<!-- cargo-rdme end -->
