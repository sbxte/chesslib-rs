#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl PieceType {
    pub(crate) fn to_group(self) -> PieceTypeGroup {
        PieceTypeGroup::from(self)
    }
}

impl From<char> for PieceType {
    fn from(value: char) -> Self {
        match value.to_ascii_uppercase() {
            'N' => Self::Knight,
            'B' => Self::Bishop,
            'R' => Self::Rook,
            'Q' => Self::Queen,
            'K' => Self::King,
            _ => Self::Pawn,
        }
    }
}

type PieceTypeGroupInt = u8;

/// Collection of [`PieceType`]s stored bitwise
///
/// Refer to [`PieceType::to_group`]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct PieceTypeGroup(PieceTypeGroupInt);

impl std::ops::Deref for PieceTypeGroup {
    type Target = PieceTypeGroupInt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::BitOr for PieceTypeGroup {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(*self | *rhs)
    }
}

impl std::ops::BitAnd for PieceTypeGroup {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(*self & *rhs)
    }
}

impl std::ops::BitXor for PieceTypeGroup {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(*self ^ *rhs)
    }
}

impl From<PieceType> for PieceTypeGroup {
    fn from(value: PieceType) -> Self {
        Self(match value {
            PieceType::Pawn => 1 << 0,
            PieceType::Knight => 1 << 1,
            PieceType::Bishop => 1 << 2,
            PieceType::Rook => 1 << 3,
            PieceType::Queen => 1 << 4,
            PieceType::King => 1 << 5,
        })
    }
}

impl From<PieceTypeGroup> for PieceTypeGroupInt {
    fn from(value: PieceTypeGroup) -> Self {
        *value
    }
}

impl PieceTypeGroup {
    /// Returns whether this group contains the specified [`PieceType`]
    pub fn has(self, piece_type: PieceType) -> bool {
        *(self & piece_type.to_group()) > 0
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PieceColor {
    White,
    Black,
}

impl std::fmt::Display for PieceColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PieceColor::White => "White",
                PieceColor::Black => "Black",
            }
        )
    }
}

impl PieceColor {
    pub fn sign(&self) -> i8 {
        match self {
            Self::White => 1,
            Self::Black => -1,
        }
    }

    pub fn invert(&self) -> Self {
        match self {
            Self::White => Self::Black,
            Self::Black => Self::White,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Piece {
    pub piece_type: PieceType,
    pub piece_color: PieceColor,
}

impl Piece {
    pub fn new(piece_type: PieceType, piece_color: PieceColor) -> Self {
        Self {
            piece_type,
            piece_color,
        }
    }
}
