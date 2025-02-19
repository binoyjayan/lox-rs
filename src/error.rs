use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

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

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecognized = &self.src[..=self.err_span.offset()];
        until_unrecognized.lines().count()
    }
}
