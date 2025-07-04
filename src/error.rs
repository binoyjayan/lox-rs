use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token '{token}' in input")]
pub struct SingleTokenError {
    // The `Source` that miette will use.
    #[source_code]
    pub src: String,
    // The token that caused the error.
    pub token: char,
    // The span of the token.
    #[label = "this input character"]
    pub err_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringTermError {
    // The `Source` that miette will use.
    #[source_code]
    pub src: String,
    // The span of the token.
    #[label = "this input character"]
    pub err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecognized = &self.src[..=self.err_span.offset()];
        until_unrecognized.lines().count()
    }
}

impl StringTermError {
    pub fn line(&self) -> usize {
        let until_unterminated = &self.src[..=self.err_span.offset()];
        until_unterminated.lines().count()
    }
}
