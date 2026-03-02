use crate::game::Move;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MoveTree {
    data: Vec<MoveNode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MoveNode {
    pub gmove: Move,
    next_moves: Vec<usize>,
}

impl MoveNode {
    pub fn next_moves(&self, tree: &MoveTree) -> impl Iterator {
        self.next_moves.iter().map(|i| &tree.data[*i])
    }
}
