//!
//! A CAN database (dbc) format parser written with Rust's nom parser combinator library.
//! CAN databases are used to exchange details about a CAN network.
//! E.g. what messages are being send over the CAN bus and what data do they contain.
//!
//! ```rust
//! use can_dbc::DBC;
//! use codegen::Scope;
//!
//! use std::fs::File;
//! use std::io;
//! use std::io::prelude::*;
//!
//! fn main() -> io::Result<()> {
//!     let mut f = File::open("./examples/sample.dbc")?;
//!     let mut buffer = Vec::new();
//!     f.read_to_end(&mut buffer)?;
//!
//!     let dbc = can_dbc::DBC::from_slice(&buffer).expect("Failed to parse dbc file");
//!
//!     let mut scope = Scope::new();
//!     for message in dbc.messages() {
//!         for signal in message.signals() {
//!
//!             let mut scope = Scope::new();
//!             let message_struct = scope.new_struct(message.message_name());
//!             for signal in message.signals() {
//!                 message_struct.field(signal.name().to_lowercase().as_str(), "f64");
//!             }
//!         }
//!     }
//!
//!     println!("{}", scope.to_string());
//!     Ok(())
//! }
//! ```

#[cfg(feature = "with-serde")]
extern crate serde;
#[cfg(feature = "with-serde")]
#[macro_use]
extern crate serde_derive;

pub mod parser;
pub mod dbc;
pub mod message;
pub mod signal;
pub mod nodes;
pub mod env_variables;
pub mod attributes;

pub use crate::dbc::DBC;

use nom::IResult;

trait DBCString {
    fn dbc_string(&self) -> String;

    // fn parse<T: DBCString>(s: &str) -> IResult<&str, T>;

    fn parse(s: &str) -> IResult<&str, Self>
    where
        Self: Sized;
}

/// Possible error cases for `can-dbc`
#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum Error<'a> {
    /// Remaining String, the DBC was only read partially.
    /// Occurs when e.g. an unexpected symbol occurs.
    Incomplete(DBC, &'a str),
    /// Parser failed
    Nom(nom::Err<nom::error::Error<&'a str>>),
    /// Can't Lookup multiplexors because the message uses extended multiplexing.
    MultipleMultiplexors,
}
