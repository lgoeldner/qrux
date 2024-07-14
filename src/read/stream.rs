#[derive(Debug, Clone)]
#[allow(clippy::module_name_repetitions)]
pub struct TokenStream<'a> {
    tokens: Vec<&'a str>,
    pos: usize,
}

#[derive(Clone, Copy)]
pub struct Backup(usize);

impl std::fmt::Debug for Backup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Backup(pos: {})", self.0)
    }
}

impl TokenStream<'_> {
    pub fn next(&mut self) -> Option<&str> {
        (self.pos < self.tokens.len()).then(|| {
            self.pos += 1;
            self.tokens[self.pos - 1]
        })
    }

	pub fn is_eof(&self) -> bool {
		self.pos >= self.tokens.len()
	}

    pub const fn save(&self) -> Backup {
        Backup(self.pos)
    }

    pub fn restore(&mut self, backup: Backup) {
        self.pos = backup.0;
    }

    pub fn peek(&self) -> Option<&str> {
        self.tokens.get(self.pos).copied()
    }

    pub fn back(&mut self) {
        self.pos -= 1;
    }

    pub const fn pos(&self) -> usize {
        self.pos
    }

    pub fn skip(&mut self, n: usize) {
        self.pos += n;
    }
}

impl<'a> FromIterator<&'a str> for TokenStream<'a> {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Self {
            tokens: iter.into_iter().collect(),
            pos: 0,
        }
    }
}
