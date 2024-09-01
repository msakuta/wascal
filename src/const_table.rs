use std::{collections::HashMap, io::Write};

#[derive(Default)]
pub(crate) struct ConstTable {
    base_addr: usize,
    buf: Vec<u8>,
    /// Constant table, we would like to represent a slice into `buf`, but we cannot have self-referential structs.
    consts: HashMap<String, (usize, usize)>,
}

const PTR_SIZE: usize = std::mem::size_of::<i32>();

impl ConstTable {
    pub fn new() -> Self {
        let mut buf = vec![];
        buf.extend_from_slice(&0u32.to_le_bytes());
        Self {
            base_addr: 0,
            buf,
            ..Self::default()
        }
    }

    pub fn _add_const(&mut self, name: impl Into<String>, value: &[u8]) -> (usize, usize) {
        let ptr = self.buf.len() + self.base_addr;
        self.buf.extend_from_slice(value);
        let ret = (ptr, value.len());
        self.consts.insert(name.into(), ret);
        ret
    }

    /// Strings are encoded as [len, bytes...] unlike Rust, because in this way we can represent it with a pointer.
    pub fn add_str(&mut self, name: impl Into<String>, value: &str) -> usize {
        // Round up to I32 alignment
        let ptr = (self.buf.len() + self.base_addr + PTR_SIZE - 1) / PTR_SIZE * PTR_SIZE;
        // Pad the gap
        self.buf.resize(ptr, 0);
        self.buf
            .extend_from_slice(&(value.len() as u32).to_le_bytes());
        self.buf.extend_from_slice(value.as_bytes());
        let ret = (ptr, value.len());
        self.consts.insert(name.into(), ret);
        ptr
    }

    pub fn base_addr(&self) -> usize {
        self.base_addr
    }

    pub fn data(&self) -> &[u8] {
        &self.buf
    }

    pub fn finish(&mut self) {
        let bytes = (self.buf.len() as u32).to_le_bytes();
        self.buf[..PTR_SIZE].copy_from_slice(&bytes);
    }

    pub fn print_data(&self, f: &mut impl Write) -> std::io::Result<()> {
        const LINE_CHARS: usize = 16;

        fn print_ascii(f: &mut impl Write, buf: &[u8]) -> std::io::Result<()> {
            for c in buf {
                if c.is_ascii() && !c.is_ascii_control() {
                    write!(f, "{}", *c as char)?;
                } else {
                    write!(f, ".")?;
                }
            }
            Ok(())
        }

        for (i, b) in self.buf.iter().enumerate() {
            if i % LINE_CHARS == 0 {
                write!(f, "\n{:#06x}: ", i)?;
            }
            let low = ('0'..='9')
                .chain('a'..='f')
                .nth((*b % 16) as usize)
                .unwrap();
            let high = ('0'..='9')
                .chain('a'..='f')
                .nth(((*b >> 4) % 16) as usize)
                .unwrap();
            write!(f, "{}{} ", high, low)?;
            if i % LINE_CHARS == LINE_CHARS - 1 {
                print_ascii(
                    f,
                    &self.buf[i / LINE_CHARS * LINE_CHARS..(i / LINE_CHARS + 1) * LINE_CHARS],
                )?;
            }
        }
        if self.buf.len() % LINE_CHARS != 0 {
            let prev_boundary = self.buf.len() / LINE_CHARS * LINE_CHARS;
            let skipped = self.buf.len() - prev_boundary;
            write!(f, "{}", "   ".repeat(skipped))?;
            print_ascii(f, &self.buf[prev_boundary..])?;
        }
        writeln!(f, "")?;
        Ok(())
    }
}
