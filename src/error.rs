use crate::path::Path;
use serde::{de, ser};
use std::error;
use std::fmt::{self, Debug, Display};
use std::io;
use std::result;
use std::str;
use std::string;
use yaml_rust::emitter;
use yaml_rust::scanner::{self, Marker, ScanError};

/// This type represents all possible errors that can occur when serializing or
/// deserializing YAML data.
pub enum Error {
    Message(String, Option<Pos>),

    Emit(emitter::EmitError),
    Scan(scanner::ScanError),
    Io(io::Error),
    Utf8(str::Utf8Error),
    FromUtf8(string::FromUtf8Error),

    EndOfStream,
    MoreThanOneDocument,
    RecursionLimitExceeded,
}

/// Alias for a `Result` with the error type `serde_yaml::Error`.
pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub struct Pos {
    marker: Marker,
    path: String,
}

/// This type represents the location that an error occured.
#[derive(Debug)]
pub struct Location {
    index: usize,
    line: usize,
    column: usize,
}

impl Location {
    /// The byte index of the error
    pub fn index(&self) -> usize {
        self.index
    }

    /// The line of the error
    pub fn line(&self) -> usize {
        self.line
    }

    /// The column of the error
    pub fn column(&self) -> usize {
        self.column
    }

    // This is to keep decoupled with the yaml crate
    #[doc(hidden)]
    fn from_marker(marker: &Marker) -> Self {
        Location {
            // `col` returned from the `yaml` crate is 0-indexed but all error messages add + 1 to this value
            column: marker.col() + 1,
            index: marker.index(),
            line: marker.line(),
        }
    }
}

impl Error {
    /// Returns the Location from the error if one exists.
    ///
    /// Not all types of errors have a location so this can return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use serde_yaml::{Value, Error};
    /// #
    /// // The `@` character as the first character makes this invalid yaml
    /// let invalid_yaml: Result<Value, Error> = serde_yaml::from_str("@invalid_yaml");
    ///
    /// let location = invalid_yaml.unwrap_err().location().unwrap();
    ///
    /// assert_eq!(location.line(), 1);
    /// assert_eq!(location.column(), 1);
    /// ```
    pub fn location(&self) -> Option<Location> {
        match self {
            Error::Message(_, Some(pos)) => Some(Location::from_marker(&pos.marker)),
            Error::Scan(scan) => Some(Location::from_marker(scan.marker())),
            _ => None,
        }
    }
}

pub(crate) fn end_of_stream() -> Error {
    Error::EndOfStream
}

pub(crate) fn more_than_one_document() -> Error {
    Error::MoreThanOneDocument
}

pub(crate) fn io(err: io::Error) -> Error {
    Error::Io(err)
}

pub(crate) fn emitter(err: emitter::EmitError) -> Error {
    Error::Emit(err)
}

pub(crate) fn scanner(err: scanner::ScanError) -> Error {
    Error::Scan(err)
}

pub(crate) fn str_utf8(err: str::Utf8Error) -> Error {
    Error::Utf8(err)
}

pub(crate) fn string_utf8(err: string::FromUtf8Error) -> Error {
    Error::FromUtf8(err)
}

pub(crate) fn recursion_limit_exceeded() -> Error {
    Error::RecursionLimitExceeded
}

pub(crate) fn fix_marker(mut error: Error, marker: Marker, path: Path) -> Error {
    if let Error::Message(_, ref mut none @ None) = &mut error {
        *none = Some(Pos {
            marker,
            path: path.to_string(),
        });
    }
    error
}

impl error::Error for Error {
    // TODO: deprecated, remove in next major version.
    #[allow(deprecated)]
    fn description(&self) -> &str {
        match self {
            Error::Message(msg, _) => &msg,
            Error::Emit(_) => "emit error",
            Error::Scan(_) => "scan error",
            Error::Io(err) => err.description(),
            Error::Utf8(err) => err.description(),
            Error::FromUtf8(err) => err.description(),
            Error::EndOfStream => "EOF while parsing a value",
            Error::MoreThanOneDocument => {
                "deserializing from YAML containing more than one document is not supported"
            }
            Error::RecursionLimitExceeded => "recursion limit exceeded",
        }
    }

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Error::Scan(err) => Some(err),
            Error::Io(err) => Some(err),
            Error::Utf8(err) => Some(err),
            Error::FromUtf8(err) => Some(err),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg, None) => Display::fmt(&msg, f),
            Error::Message(msg, Some(Pos { marker, path })) => {
                if path == "." {
                    write!(f, "{}", ScanError::new(*marker, &msg))
                } else {
                    write!(f, "{}: {}", path, ScanError::new(*marker, &msg))
                }
            }
            Error::Emit(emitter::EmitError::FmtError(_)) => f.write_str("yaml-rust fmt error"),
            Error::Emit(emitter::EmitError::BadHashmapKey) => f.write_str("bad hash map key"),
            Error::Scan(err) => Display::fmt(&err, f),
            Error::Io(err) => Display::fmt(&err, f),
            Error::Utf8(err) => Display::fmt(&err, f),
            Error::FromUtf8(err) => Display::fmt(&err, f),
            Error::EndOfStream => f.write_str("EOF while parsing a value"),
            Error::MoreThanOneDocument => f.write_str(
                "deserializing from YAML containing more than one document is not supported",
            ),
            Error::RecursionLimitExceeded => f.write_str("recursion limit exceeded"),
        }
    }
}

// Remove two layers of verbosity from the debug representation. Humans often
// end up seeing this representation because it is what unwrap() shows.
impl Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg, pos) => formatter
                .debug_tuple("Message")
                .field(&msg)
                .field(&pos)
                .finish(),
            Error::Emit(emit) => formatter.debug_tuple("Emit").field(&emit).finish(),
            Error::Scan(scan) => formatter.debug_tuple("Scan").field(&scan).finish(),
            Error::Io(io) => formatter.debug_tuple("Io").field(&io).finish(),
            Error::Utf8(utf8) => formatter.debug_tuple("Utf8").field(&utf8).finish(),
            Error::FromUtf8(from_utf8) => {
                formatter.debug_tuple("FromUtf8").field(&from_utf8).finish()
            }
            Error::EndOfStream => formatter.debug_tuple("EndOfStream").finish(),
            Error::MoreThanOneDocument => formatter.debug_tuple("MoreThanOneDocument").finish(),
            Error::RecursionLimitExceeded => {
                formatter.debug_tuple("RecursionLimitExceeded").finish()
            }
        }
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string(), None)
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string(), None)
    }
}
