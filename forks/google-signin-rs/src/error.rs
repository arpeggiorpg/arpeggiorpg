use std::{self, fmt, io};
use serde_json;

/// A network or validation error
#[derive(Debug)]
pub enum Error {
    DecodeJson(serde_json::Error),
    JSONWebToken(Box<dyn std::error::Error + Send + Sync + 'static>),
    ConnectionError(Box<dyn std::error::Error + Send + Sync + 'static>),
    InvalidKey,
    InvalidToken,
    InvalidIssuer,
    InvalidAudience,
    InvalidHostedDomain,
    ExpiredToken,
}

impl std::error::Error for Error {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match *self {
            Error::DecodeJson(ref err) => Some(err),
            Error::ConnectionError(ref err) => Some(&**err),
            _ => None,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::DecodeJson(ref err) => err.fmt(f),
            Error::ConnectionError(ref err) => err.fmt(f),
            Error::JSONWebToken(ref err) => err.fmt(f),
            Error::InvalidKey => f.write_str("Token does not match any known key"),
            Error::InvalidToken => f.write_str("Token was not recognized by google"),
            Error::InvalidIssuer => f.write_str("Token was not issued by google"),
            Error::InvalidAudience => f.write_str("Token is for a different google application"),
            Error::InvalidHostedDomain => f.write_str("User is not a member of the hosted domain(s)"),
            Error::ExpiredToken => f.write_str("Token is expired"),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::ConnectionError(Box::new(err))
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Error {
        Error::DecodeJson(err)
    }
}

impl From<jwt_compact::ParseError> for Error {
    fn from(err: jwt_compact::ParseError) -> Error {
        Error::JSONWebToken(Box::new(err))
    }
}


impl From<reqwest::Error> for Error {
    fn from(err: reqwest::Error) -> Error {
        Error::ConnectionError(Box::new(err))
    }
}
