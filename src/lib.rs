//! Lightweight declarative macro for sets of strings.
//!
//! ```
//! strum_lite::strum! {
//!     pub enum Casing {
//!         Kebab = "kebab-case",
//!         ScreamingSnake = "SCREAMING_SNAKE",
//!     }
//! }
//! ```
//!
//! # Features
//! - Implements [`FromStr`](core::str::FromStr) and [`Display`](core::fmt::Display).
//! - Attributes (docs, `#[derive(..)]`s) are passed through to the definition and variants.
//! - Aliases are supported.
//! - Custom enum discriminants are passed through.
//! - Generated code is `#![no_std]`.
//! - The generated [`FromStr::Err`](core::str::FromStr) provides a helpful error message.
//! - You may ask for a custom error type rather than using this crate's [`ParseError`].
//!
//! ```
//! strum_lite::with_error! {
//!     #[derive(Default)]
//!     #[repr(u8)]
//!     pub enum Casing {
//!         Kebab = "kebab-case" | "kebab" = 1,
//!         #[default]
//!         ScreamingSnake = "SCREAMING_SNAKE" = 1 + 2,
//!     }
//!     throws
//!     #[derive(Clone, Copy)]
//!     ParseCasingError;
//! }
//! ```

#![no_std]

#[doc(hidden)]
pub use core;

use core::fmt;

/// Pointer-wide shared error type for [`strum!`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParseError(#[doc(hidden)] pub &'static &'static [&'static str]);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            [] => f.write_str("Uninhabited type is impossible to parse"),
            [first] => f.write_fmt(format_args!("Expected string `{first}`")),
            [first, second] => f.write_fmt(format_args!("Expected `{first}` or `{second}`")),
            [first, rest @ .., last] => {
                f.write_fmt(format_args!("Expected one of `{first}`"))?;
                for it in rest {
                    f.write_fmt(format_args!(", `{it}`"))?
                }
                f.write_fmt(format_args!(", or `{last}`"))
            }
        }
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ParseError")
            .field(&DebugWithDisplay(self))
            .finish()
    }
}

impl core::error::Error for ParseError {}

struct DebugWithDisplay<T>(T);
impl<T: fmt::Display> fmt::Debug for DebugWithDisplay<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Give the passed in enum a [`FromStr`](core::str::FromStr) and [`Display`](core::fmt::Display)
/// implementation.
///
/// ```
/// strum_lite::strum! {
///     #[derive(Default)]
///     pub enum Casing {
///         Kebab = "kebab-case" | "kebab",
///         #[default]
///         ScreamingSnake = "SCREAMING_SNAKE",
///     }
/// }
///
/// let derives_are_passed_through = Casing::default();
/// let implements_display = Casing::Kebab.to_string();
/// let implements_from_str = "kebab".parse::<Casing>().unwrap();
/// ```
///
/// See [crate documentation](mod@self) for more.
#[macro_export]
macro_rules! strum {
    (
        $(#[$enum_meta:meta])*
        $enum_vis:vis enum $enum_name:ident {
            $(
                $(#[$variant_meta:meta])*
                $variant_name:ident = $string:literal $(| $alias:literal)* $(= $discriminant:expr)?
            ),* $(,)?
        }
    ) => {
        $(#[$enum_meta])*
        $enum_vis enum $enum_name {
            $(
                $(#[$variant_meta])*
                #[doc = $crate::core::concat!(" String representation: `", $string, "`")]
                $variant_name $(= $discriminant)?,
            )*
        }
        const _: () = {
            use $crate::core;
            impl core::fmt::Display for $enum_name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    fn as_str(e: &$enum_name) -> &core::primitive::str {
                        match *e {
                            $($enum_name::$variant_name => $string),*
                        }
                    }
                    core::fmt::Formatter::write_str(f, as_str(self))
                }
            }
            impl core::str::FromStr for $enum_name {
                type Err = $crate::ParseError;
                fn from_str(s: &core::primitive::str) -> core::result::Result<Self, $crate::ParseError> {
                    const ALL: &[&core::primitive::str] = &[$($string),*];
                    match s {
                        $(
                            $string $(| $alias )* => core::result::Result::Ok(Self::$variant_name),
                        )*
                        _ => core::result::Result::Err($crate::ParseError(&ALL))
                    }
                }
            }
        };
    };
}

/// Like [`strum!`], but also declares a custom zero-sized error type,
/// rather than using the pointer-wide [`ParseError`].
///
/// The error type will implement [`Debug`](core::fmt::Debug),
/// [`Display`](core::fmt::Debug), and [`Error`](core::error::Error),
/// as well as custom derives.
///
/// ```
/// strum_lite::with_error! {
///     #[derive(Default)]
///     pub enum Casing {
///         Kebab = "kebab-case" | "kebab",
///         #[default]
///         ScreamingSnake = "SCREAMING_SNAKE",
///     }
///     throws #[derive(Default)] ParseCasingError;
/// }
/// ```
///
/// See [crate documentation](mod@self) for more.
///
/// You are encouraged to copy-paste this macro into your own code.
#[macro_export]
macro_rules! with_error {
    (
        $(#[$enum_meta:meta])*
        $enum_vis:vis enum $enum_name:ident {
            $(
                $(#[$variant_meta:meta])*
                $variant_name:ident = $string:literal $(| $alias:literal)* $(= $discriminant:expr)?
            ),* $(,)?
        }
        throws
        $(#[$error_meta:meta])*
        $error_name:ident $(;)?
    ) => {
        $(#[$enum_meta])*
        $enum_vis enum $enum_name {
            $(
                $(#[$variant_meta])*
                #[doc = $crate::core::concat!(" String representation: `", $string, "`")]
                $variant_name $(= $discriminant)?,
            )*
        }

        const _: () = {
            use $crate::core;
            impl core::fmt::Display for $enum_name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    fn as_str(e: &$enum_name) -> &core::primitive::str {
                        match *e {
                            $($enum_name::$variant_name => $string),*
                        }
                    }
                    core::fmt::Formatter::write_str(f, as_str(self))
                }
            }

            impl core::str::FromStr for $enum_name {
                type Err = $error_name;
                fn from_str(s: &core::primitive::str) -> core::result::Result<Self, $error_name> {
                    match s {
                        $(
                            $string $(| $alias )* => core::result::Result::Ok(Self::$variant_name),
                        )*
                        _ => core::result::Result::Err($error_name)
                    }
                }
            }
        };

        $(#[$error_meta])*
        #[doc = $crate::core::concat!(" Error returned when parsing [`", $crate::core::stringify!($enum_name), "`] from a string.")]
        $enum_vis struct $error_name;

        const _: () = {
            use $crate::core;
            impl core::fmt::Display for $error_name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    let all: &[&core::primitive::str] = &[
                        $($string),*
                    ];
                    let write_fmt = core::fmt::Formatter::write_fmt;
                    match all {
                        [] => core::fmt::Formatter::write_str(f, "Uninhabited type is impossible to parse"),
                        [first] => write_fmt(f, core::format_args!("Expected string `{}`", first)),
                        [first, second] => write_fmt(f, core::format_args!("Expected `{}` or `{}`", first, second)),
                        [first, rest @ .., last] => {
                            write_fmt(f, core::format_args!("Expected one of `{}`", first))?;
                            for it in rest {
                                write_fmt(f, core::format_args!(", `{}`", it))?
                            }
                            write_fmt(f, core::format_args!(", or `{}`", last))
                        }
                    }
                }
            }
            impl core::fmt::Debug for $error_name {
                fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    use core::fmt;
                    struct DebugWithDisplay<T>(T);
                    impl<T: fmt::Display> fmt::Debug for DebugWithDisplay<T> {
                        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                            self.0.fmt(f)
                        }
                    }
                    let mut f = fmt::Formatter::debug_tuple(f, core::stringify!($error_name));
                    fmt::DebugTuple::field(&mut f, &DebugWithDisplay(self));
                    fmt::DebugTuple::finish(&mut f)
                }
            }
            impl core::error::Error for $error_name {}
        };
    };
}
