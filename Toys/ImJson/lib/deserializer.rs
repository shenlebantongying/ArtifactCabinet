pub struct Deserializer<'a> {
    current_pos: usize,
    s: &'a str,
}

impl<'a> Deserializer<'a> {
    pub fn new(s: &str) -> Deserializer {
        Deserializer { current_pos: 0, s }
    }

    pub fn get_next_token(&mut self) -> Option<&str> {
        if self.current_pos == self.s.len() {
            return None;
        }

        // Skip WhiteSpace
        if self.current_char().is_ascii_whitespace() {
            let advance = self.seek_none_whitespace(self.current_pos + 1);
            self.current_pos += advance + 1;
        }

        let current_char = self.current_char();
        let old_pos = self.current_pos;

        match current_char {
            b'{' | b'}' | b':' | b'[' | b']' | b',' => {
                self.current_pos += 1;
                return Some(&self.s[old_pos..old_pos + 1]);
            }
            b'\"' => {
                let advance = self.seek_str_end(old_pos + 1);
                self.current_pos += advance + 2;
                return Some(&self.s[old_pos..self.current_pos]);
            }
            _ => {
                if current_char.is_ascii_digit() {
                    let advance = self.seek_number_end(old_pos + 1);
                    self.current_pos += advance + 1;
                    return Some(&self.s[old_pos..self.current_pos]);
                }
            }
        }

        panic!()
    }

    pub fn expect_begin_array(&mut self) -> Option<&str> {
        let str = self.get_next_token();
        match str {
            Some("[") => str,
            _ => None,
        }
    }

    pub fn expect_end_array(&mut self) -> Option<&str> {
        let str = self.get_next_token();
        match str {
            Some("]") => str,
            _ => None,
        }
    }

    pub fn expect_i64(&mut self) -> Option<i64> {
        let str = self.get_next_token()?;
        if str == "," {
            self.expect_i64()
        } else {
            str.parse::<i64>().ok()
        }
    }

    fn current_char(&self) -> u8 {
        self.s.as_bytes()[self.current_pos]
    }

    fn seek_str_end(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if *c == b'\"' {
                return i;
            }
        }
        todo!();
    }

    fn seek_none_whitespace(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if !c.is_ascii_whitespace() {
                return i;
            }
        }
        todo!();
    }

    fn seek_number_end(&self, start: usize) -> usize {
        for (i, c) in self.s.as_bytes()[start..self.s.len()].iter().enumerate() {
            if !c.is_ascii_digit() {
                return i;
            }
        }
        todo!()
    }
}
