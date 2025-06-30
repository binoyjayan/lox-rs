pub struct Parser<'de> {
    whole: &'de str,
    scanner: Scanner<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            scanner: Scanner::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Token<'de>>, Error> {
        let mut tokens = Vec::new();
        for token in &mut self.scanner {
            match token {
                Ok(t) => tokens.push(t),
                Err(e) => return Err(e),
            }
        }
        Ok(tokens)
    }
}
