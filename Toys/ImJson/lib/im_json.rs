pub struct J<'a> {
    cur: usize,
    s: &'a str,
}

impl<'a> J<'a> {
    pub fn new(s: &str) -> J {
        J { cur: 0, s }
    }

    pub fn get_next_token(&mut self) -> Option<&str> {
        if self.cur == self.s.len() {
            return None;
        }

        // Skip WhiteSpace
        if self.s.as_bytes()[self.cur].is_ascii_whitespace() {
            let advance = self.get_next_none_whitespace(self.cur + 1);
            self.cur += advance + 1;
        }

        let next_char = self.s.as_bytes()[self.cur];
        let cur = self.cur;

        match next_char {
            b'{' | b'}' | b':' | b'[' | b']' | b',' => {
                self.cur += 1;
                return Some(&self.s[cur..cur + 1]);
            }
            b'\"' => {
                let advance = self.get_next_str(cur + 1);
                self.cur += advance + 2;
                return Some(&self.s[cur..self.cur]);
            }
            _ => {
                if next_char.is_ascii_digit() {
                    let advance = self.get_number(cur + 1);
                    self.cur += advance + 1;
                    return Some(&self.s[cur..self.cur]);
                }
            }
        }

        panic!()
    }

    fn get_next_str(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if *c == b'\"' {
                return i;
            }
        }
        todo!();
    }

    fn get_next_none_whitespace(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if !c.is_ascii_whitespace() {
                return i;
            }
        }
        todo!();
    }

    fn get_number(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if !c.is_ascii_digit() {
                return i;
            }
        }
        todo!()
    }
}
