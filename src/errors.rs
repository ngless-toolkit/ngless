//! Error types, mirroring `NGLess/NGLess/NGError.hs`.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NgErrorType {
    /// A bug in NGLess itself.
    ShouldNotOccur,
    /// A bug in the user's script.
    ScriptError,
    /// Bad input data.
    DataError,
    /// An IO/system failure.
    SystemError,
    /// A generic error.
    GenericError,
    /// Exit without an error status.
    NoErrorExit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NgError {
    pub kind: NgErrorType,
    pub message: String,
}

impl NgError {
    pub fn new(kind: NgErrorType, message: impl Into<String>) -> Self {
        NgError {
            kind,
            message: message.into(),
        }
    }
    pub fn script(message: impl Into<String>) -> Self {
        NgError::new(NgErrorType::ScriptError, message)
    }
    pub fn should_not_occur(message: impl Into<String>) -> Self {
        NgError::new(NgErrorType::ShouldNotOccur, message)
    }
}

impl fmt::Display for NgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for NgError {}

pub type NgResult<T> = Result<T, NgError>;
