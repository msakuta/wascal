use std::collections::HashMap;

#[derive(Default)]
pub(crate) struct ConstTable {
    base_addr: usize,
    buf: Vec<u8>,
    /// Constant table, we would like to represent a slice into `buf`, but we cannot have self-referential structs.
    consts: HashMap<String, (usize, usize)>,
}

impl ConstTable {
    pub fn new() -> Self {
        Self {
            base_addr: 0x800,
            ..Self::default()
        }
    }

    pub fn add_const(&mut self, name: impl Into<String>, value: &[u8]) -> (usize, usize) {
        let ptr = self.buf.len() + self.base_addr;
        self.buf.extend_from_slice(value);
        let ret = (ptr, value.len());
        self.consts.insert(name.into(), ret);
        ret
    }

    pub fn base_addr(&self) -> usize {
        self.base_addr
    }

    pub fn data(&self) -> &[u8] {
        &self.buf
    }
}
