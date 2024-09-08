use std::collections::HashSet;

/// Conceptual set of structs that could be any struct.
#[derive(Debug, Clone, PartialEq)]
pub enum StructSet {
    Any,
    Some(HashSet<String>),
}

impl Default for StructSet {
    fn default() -> Self {
        Self::Some(HashSet::new())
    }
}

impl<'a> IntoIterator for &'a StructSet {
    type IntoIter = StructSetIter<'a>;
    type Item = &'a String;
    fn into_iter(self) -> Self::IntoIter {
        match self {
            StructSet::Any => StructSetIter(None),
            StructSet::Some(structs) => StructSetIter(Some(structs.iter())),
        }
    }
}

pub struct StructSetIter<'a>(Option<std::collections::hash_set::Iter<'a, String>>);

impl<'a> Iterator for StructSetIter<'a> {
    type Item = &'a String;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.as_mut().and_then(|it| it.next())
    }
}

impl StructSet {
    pub fn is_empty(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Some(structs) => structs.is_empty(),
        }
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }

    pub fn is_some(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Some(structs) => !structs.is_empty(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Any => std::usize::MAX,
            Self::Some(structs) => structs.len(),
        }
    }

    pub fn first(&self) -> Option<&String> {
        match self {
            Self::Any => None,
            Self::Some(structs) => structs.iter().next(),
        }
    }

    pub fn union(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Some(lhs), Self::Some(rhs)) => Self::Some(lhs.union(rhs).cloned().collect()),
            _ => Self::Any,
        }
    }

    pub fn intersection(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Some(lhs), Self::Some(rhs)) => {
                Self::Some(lhs.intersection(rhs).cloned().collect())
            }
            (Self::Any, Self::Some(rhs)) => Self::Some(rhs.clone()),
            (Self::Some(lhs), Self::Any) => Self::Some(lhs.clone()),
            _ => Self::Any,
        }
    }

    pub fn insert(&mut self, item: String) -> bool {
        match self {
            Self::Some(structs) => structs.insert(item),
            _ => false, // Always assumed to contain
        }
    }
}
