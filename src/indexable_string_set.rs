use std::{collections::hash_map::RandomState, fmt::Debug, hash::BuildHasher};

// Strings are interned (deduplicated). Two equal strings will refer to the same Rust String.
// To prevent lifetime issues, we don't store references to strings directly in Value,
// but an index into the strings vec.
// Since interned strings are only ever added, that index is stable (unlike references)
pub struct IndexableStringSet {
    strings: Vec<Option<String>>,
    hash_builder: RandomState,
}

impl IndexableStringSet {
    pub fn new() -> Self {
        Self {
            strings: vec![None; 1000],
            hash_builder: RandomState::new(),
        }
    }

    pub fn get_by_index(&self, index: usize) -> &String {
        self.strings[index].as_ref().unwrap()
    }

    fn get_insert_index_of(&mut self, string: &str) -> Result<usize, usize> {
        let mut index = self.hash(string) as usize % self.strings.len();

        let start = index;

        while let Some(existing_str) = &self.strings[index] {
            if existing_str == string {
                return Ok(index);
            } else {
                // TODO: Improve reprobing
                index = (index + 1) % self.strings.len();
                if start == index {
                    self.grow();
                }
            }
        }

        Err(index)
    }

    pub fn get_or_insert(&mut self, str: &str) -> usize {
        match self.get_insert_index_of(str) {
            Ok(existing_index) => existing_index,
            Err(insert_index) => {
                self.strings[insert_index] = Some(str.to_owned());
                insert_index
            }
        }
    }

    pub fn get_or_insert_existing(&mut self, string: String) -> usize {
        match self.get_insert_index_of(&string) {
            Ok(existing_index) => existing_index,
            Err(insert_index) => {
                self.strings[insert_index] = Some(string);
                insert_index
            }
        }
    }

    fn grow(&mut self) {
        self.strings.resize(self.strings.len() * 2, None);
    }

    fn hash(&self, string: &str) -> u64 {
        // TODO: Profile, improve if needed
        self.hash_builder.hash_one(string)
    }
}

impl Debug for IndexableStringSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for str in self.strings.iter().flatten() {
            write!(f, "{str}, ")?;
        }
        write!(f, "]")?;

        Ok(())
    }
}
