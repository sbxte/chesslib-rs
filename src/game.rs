#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl From<char> for PieceType {
    fn from(value: char) -> Self {
        match value {
            'N' => Self::Knight,
            'B' => Self::Bishop,
            'R' => Self::Rook,
            'Q' => Self::Queen,
            'K' => Self::King,
            _ => Self::Pawn,
        }
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

#[derive(Clone, Debug, PartialEq)]
pub struct Board {
    /// [col, col, ..., col]
    /// each column would have [piece, piece, ..., piece]
    /// so the grid would be [[piece, ...], [piece, ...], ...]
    /// to access a piece at row and col, use grid[row * 8 + col]
    pub grid: [Option<Piece>; 64],
    pub turn: PieceColor,
    pub winner: Option<PieceColor>,
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Board {
    pub fn new() -> Self {
        let mut board = Self {
            grid: [None; 64],
            turn: PieceColor::White,
            winner: None,
        };

        // Pawns
        for x in 0..8 {
            board.grid[x * 8 + 1] = Some(Piece::new(PieceType::Pawn, PieceColor::White));
            board.grid[x * 8 + 6] = Some(Piece::new(PieceType::Pawn, PieceColor::Black));
        }

        // Rooks
        board.grid[0] = Some(Piece::new(PieceType::Rook, PieceColor::White)); // 0 * 8 + 0
        board.grid[56] = Some(Piece::new(PieceType::Rook, PieceColor::White)); // 7 * 8 + 0
        board.grid[7] = Some(Piece::new(PieceType::Rook, PieceColor::Black)); // 0 * 8 + 7
        board.grid[63] = Some(Piece::new(PieceType::Rook, PieceColor::Black)); // 7 * 8 + 7

        // Knights
        board.grid[8] = Some(Piece::new(PieceType::Knight, PieceColor::White)); // 1 * 8 + 0
        board.grid[48] = Some(Piece::new(PieceType::Knight, PieceColor::White)); // 6 * 8 + 0
        board.grid[15] = Some(Piece::new(PieceType::Knight, PieceColor::Black)); // 1 * 8 + 7
        board.grid[55] = Some(Piece::new(PieceType::Knight, PieceColor::Black)); // 6 * 8 + 7

        // Bishops
        board.grid[16] = Some(Piece::new(PieceType::Bishop, PieceColor::White)); // 2 * 8 + 0
        board.grid[40] = Some(Piece::new(PieceType::Bishop, PieceColor::White)); // 5 * 8 + 0
        board.grid[23] = Some(Piece::new(PieceType::Bishop, PieceColor::Black)); // 2 * 8 + 7
        board.grid[47] = Some(Piece::new(PieceType::Bishop, PieceColor::Black)); // 5 * 8 + 7

        // Queens
        board.grid[32] = Some(Piece::new(PieceType::Queen, PieceColor::White)); // 4 * 8 + 0
        board.grid[39] = Some(Piece::new(PieceType::Queen, PieceColor::Black)); // 4 * 8 + 7

        // Kings
        board.grid[24] = Some(Piece::new(PieceType::King, PieceColor::White)); // 3 * 8 + 0
        board.grid[31] = Some(Piece::new(PieceType::King, PieceColor::Black)); // 3 * 8 + 7

        board
    }

    fn pos_to_idx(pos: Pos) -> usize {
        (pos.0 * 8 + pos.1) as usize
    }

    pub fn get_piece(&self, pos: Pos) -> Option<&Piece> {
        self.grid[Self::pos_to_idx(pos)].as_ref()
    }

    pub fn set_board(&mut self, other: &Self) {
        self.turn = other.turn;
        for (i, square) in self.grid.iter_mut().enumerate() {
            *square = other.grid[i];
        }
    }

    pub fn apply_move(&mut self, pmove: Move) -> Result<(), TurnErr> {
        if self.winner.is_some() {
            return Ok(());
        }
        if self.turn != pmove.piece_color {
            return Err(TurnErr(self.turn));
        }

        let piece = self.grid[Self::pos_to_idx(pmove.from)];
        self.grid[Self::pos_to_idx(pmove.from)] = None;
        self.grid[Self::pos_to_idx(pmove.to)] = piece;

        self.turn = self.turn.invert();

        if let Some(p) = pmove.captures
            && p == PieceType::King
        {
            self.winner = Some(pmove.piece_color);
        }

        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TurnErr(PieceColor);

impl std::fmt::Display for TurnErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid turn. Current turn: {}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
/// Moves must always be valid
pub struct Move {
    from: Pos,
    to: Pos,
    piece_type: PieceType,
    piece_color: PieceColor,
    captures: Option<PieceType>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MoveErr {
    MoveOpponent,
    TakeSelf,
    NoPiece,
    InvalidCapture,
    IllegalPieceMove,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MoveNotationErr {
    MoveErr(MoveErr),
    ParseNotationError(ParseNotationErr),
    IndeterminateMove, // More than one possible piece to move; Which piece to move is not clear
}

impl Move {
    pub fn new_unchecked(
        from: Pos,
        to: Pos,
        piece_type: PieceType,
        piece_color: PieceColor,
        captures: Option<PieceType>,
    ) -> Self {
        Self {
            from,
            to,
            piece_type,
            piece_color,
            captures,
        }
    }

    pub fn new(
        from: Pos,
        to: Pos,
        piece_type: PieceType,
        piece_color: PieceColor,
        captures: Option<PieceType>,
        board: &Board,
    ) -> Result<Self, MoveErr> {
        Self::is_legal(from, to, board, piece_color)?;
        Ok(Self::new_unchecked(
            from,
            to,
            piece_type,
            piece_color,
            captures,
        ))
    }

    pub fn is_legal(from: Pos, to: Pos, grid: &Board, turn: PieceColor) -> Result<(), MoveErr> {
        // Must move existing piece
        let piece_from = match grid.get_piece(from) {
            Some(p) => p,
            None => return Err(MoveErr::NoPiece),
        };
        // Cannot move opponent piece
        if piece_from.piece_color != turn {
            return Err(MoveErr::MoveOpponent);
        }
        // Cannot take own piece
        let piece_to = grid.get_piece(to);
        if let Some(piece_to) = piece_to {
            if piece_to.piece_color == turn {
                return Err(MoveErr::TakeSelf);
            }
        }

        // Legal piece movement
        let diff_y = to.1 - from.1;
        let diff_x = to.0 - from.0;
        match piece_from.piece_type {
            PieceType::Pawn => {
                if diff_y != turn.sign() {
                    return Err(MoveErr::IllegalPieceMove);
                }
                if diff_x.abs() > 1 || (diff_x.abs() == 1 && piece_to.is_some()) {
                    return Err(MoveErr::IllegalPieceMove);
                }
            }
            PieceType::Rook => {
                // Move in cardinal directions only
                if (diff_x + diff_y).abs() != (diff_x - diff_y).abs() {
                    return Err(MoveErr::IllegalPieceMove);
                }
                // Cannot jump over pieces
                let mut walk_x = 0;
                let mut walk_y = 0;
                while walk_x != diff_x && walk_y != diff_y {
                    walk_x += diff_x.signum();
                    walk_y += diff_y.signum();
                    if grid
                        .get_piece(Pos(from.0 + walk_x, from.1 + walk_y))
                        .is_some()
                    {
                        return Err(MoveErr::IllegalPieceMove);
                    }
                }
            }
            PieceType::Bishop => {
                // Move in diagonals only
                if diff_x.abs() != diff_y.abs() {
                    return Err(MoveErr::IllegalPieceMove);
                }
                // Cannot jump over pieces
                let mut walk_x = 0;
                let mut walk_y = 0;
                while walk_x != diff_x && walk_y != diff_y {
                    walk_x += diff_x.signum();
                    walk_y += diff_y.signum();
                    if grid
                        .get_piece(Pos(from.0 + walk_x, from.1 + walk_y))
                        .is_some()
                    {
                        return Err(MoveErr::IllegalPieceMove);
                    }
                }
            }
            PieceType::Queen => {
                // Move in only cardinal or diagonal
                if diff_x.abs() != diff_y.abs()
                    || (diff_x + diff_y).abs() != (diff_x - diff_y).abs()
                {
                    return Err(MoveErr::IllegalPieceMove);
                }

                // Cannot jump over pieces
                let mut walk_x = 0;
                let mut walk_y = 0;
                while walk_x != diff_x && walk_y != diff_y {
                    walk_x += diff_x.signum();
                    walk_y += diff_y.signum();
                    if grid
                        .get_piece(Pos(from.0 + walk_x, from.1 + walk_y))
                        .is_some()
                    {
                        return Err(MoveErr::IllegalPieceMove);
                    }
                }
            }
            PieceType::King => {
                // Maximum distance in x or y of 1
                if diff_y.abs() > 1 || diff_x.abs() > 1 {
                    return Err(MoveErr::IllegalPieceMove);
                }
            }
            PieceType::Knight => {
                // Move in L shape only
                if diff_x.abs() > 2 || diff_y.abs() > 2 {
                    return Err(MoveErr::IllegalPieceMove);
                }
                if !((diff_x.abs() == 2 && diff_y.abs() == 1)
                    || (diff_x.abs() == 1 && diff_y.abs() == 2))
                {
                    return Err(MoveErr::IllegalPieceMove);
                }
            }
        };
        Ok(())
    }

    /// Input string must already be trimmed and sanitized
    pub fn parse_notation(
        input: &str,
        board: &Board,
        turn: PieceColor,
    ) -> Result<Self, MoveNotationErr> {
        if input.len() < 3 {
            return Err(MoveNotationErr::ParseNotationError(
                ParseNotationErr::TooShort,
            ));
        }
        let piece_type = PieceType::from(input.chars().nth(0).unwrap());
        let input = if let PieceType::Pawn = piece_type {
            input
        } else {
            &input[1..]
        };

        let is_capture = matches!(input.chars().nth(0).unwrap(), 'x');
        let input = if is_capture { &input[1..] } else { input };

        let to = match Pos::from_notation(&input[input.len() - 2..]) {
            Err(x) => return Err(MoveNotationErr::ParseNotationError(x)),
            Ok(x) => x,
        };
        let captures = if is_capture {
            match board.get_piece(to) {
                Some(x) => Some(x.piece_type),
                None => return Err(MoveNotationErr::MoveErr(MoveErr::InvalidCapture)),
            }
        } else {
            None
        };

        // Tricky part, infer where the piece moved from
        let from_partial = PartialPos::from_coord(&input[..input.len() - 2]);
        if let PartialPos(Some(col), Some(row)) = from_partial {
            let f = Pos(col, row);
            let p = match board.get_piece(f) {
                None => return Err(MoveNotationErr::MoveErr(MoveErr::NoPiece)),
                Some(x) => x,
            };
            return Self::new(f, to, p.piece_type, p.piece_color, captures, board)
                .map_err(MoveNotationErr::MoveErr);
        }

        let mut found = 0;
        let mut from = None;
        macro_rules! check {
            ($x: expr, $y: expr) => {
                if from.is_some() {
                    return Err(MoveNotationErr::IndeterminateMove);
                }
                if let Some(f) = Pos::new_bounded($x, $y)
                    && let Some(p) = board.get_piece(f)
                    && p.piece_type == piece_type
                    && p.piece_color == turn
                {
                    found += 1;
                    from = Some(f);
                }
            };
        }
        match piece_type {
            PieceType::Pawn => {
                if let PartialPos(Some(col), _) = from_partial {
                    check!(col, to.1 - turn.sign());
                } else {
                    check!(to.0, to.1 - turn.sign());
                };
            }
            PieceType::King => {
                for c in (0.max(to.0 - 1))..(8.min(to.0 + 1)) {
                    for r in (0.max(to.1 - 1))..(8.min(to.1 + 1)) {
                        check!(c, r);
                    }
                }
            }
            PieceType::Rook => match from_partial {
                PartialPos(Some(col), _) => {
                    check!(col, to.1);
                }
                PartialPos(_, Some(row)) => {
                    check!(to.0, row);
                }
                _ => {
                    for r in 0..8 {
                        check!(to.0, r);
                    }
                    for c in 0..8 {
                        check!(c, to.1);
                    }
                }
            },
            PieceType::Bishop => match from_partial {
                PartialPos(Some(col), _) => {
                    let diff = to.0 - col;
                    check!(col, to.1 + diff);
                    check!(col, to.1 - diff);
                }
                PartialPos(_, Some(row)) => {
                    let diff = to.1 - row;
                    check!(to.0 + diff, row);
                    check!(to.0 - diff, row);
                }
                _ => {
                    for step in 0..8 {
                        check!(to.0 + step, to.1 + step);
                        check!(to.0 - step, to.1 + step);
                        check!(to.0 + step, to.1 - step);
                        check!(to.0 - step, to.1 - step);
                    }
                }
            },
            PieceType::Queen => match from_partial {
                PartialPos(Some(col), _) => {
                    // Check cardinals
                    check!(col, to.1);
                    // Check diagonals
                    let diff = to.0 - col;
                    check!(col, to.1 + diff);
                    check!(col, to.1 - diff);
                }
                PartialPos(_, Some(row)) => {
                    // Check cardinals
                    check!(to.0, row);
                    // Check diagonals
                    let diff = to.1 - row;
                    check!(to.0 + diff, row);
                    check!(to.0 - diff, row);
                }
                _ => {
                    for step in 0..8 {
                        // Check cardinals
                        check!(to.0 - step, to.1);
                        check!(to.0 + step, to.1);
                        check!(to.0, to.1 - step);
                        check!(to.0, to.1 + step);
                        // Check diagonals
                        check!(to.0 - step, to.1 - step);
                        check!(to.0 + step, to.1 - step);
                        check!(to.0 - step, to.1 + step);
                        check!(to.0 + step, to.1 + step);
                    }
                }
            },
            PieceType::Knight => match from_partial {
                PartialPos(Some(col), _) => {
                    let diff = 3 - (col - to.0).abs();
                    check!(col, to.1 - diff);
                    check!(col, to.1 + diff);
                }
                PartialPos(_, Some(row)) => {
                    let diff = 3 - (row - to.1).abs();
                    check!(to.0 - diff, row);
                    check!(to.0 + diff, row);
                }
                _ => {
                    check!(to.0 - 2, to.1 - 1);
                    check!(to.0 - 2, to.1 + 1);
                    check!(to.0 - 1, to.1 - 2);
                    check!(to.0 - 1, to.1 + 2);
                    check!(to.0 + 2, to.1 - 1);
                    check!(to.0 + 2, to.1 + 1);
                    check!(to.0 + 1, to.1 - 2);
                    check!(to.0 + 1, to.1 + 2);
                }
            },
        };
        if found > 1 {
            return Err(MoveNotationErr::IndeterminateMove);
        }
        if from.is_none() {
            return Err(MoveNotationErr::MoveErr(MoveErr::IllegalPieceMove));
        }

        // Check legality by using `Self::new`
        match Self::new(from.unwrap(), to, piece_type, turn, captures, board) {
            Err(x) => Err(MoveNotationErr::MoveErr(x)),
            Ok(x) => Ok(x),
        }
    }
}

impl std::fmt::Display for MoveErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Move Error: {}",
            match self {
                MoveErr::MoveOpponent => "Cannot move opponent's piece",
                MoveErr::TakeSelf => "Cannot take your own piece",
                MoveErr::NoPiece => "No pieces to move",
                MoveErr::InvalidCapture => "Invalid capture",
                MoveErr::IllegalPieceMove => "Illegal move",
            }
        )
    }
}

impl std::fmt::Display for MoveNotationErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Move Notation Error: {}",
            match self {
                MoveNotationErr::MoveErr(e) => format!("{}", e),
                MoveNotationErr::ParseNotationError(e) => format!("{}", e),
                MoveNotationErr::IndeterminateMove => "Indeterminate move".to_string(),
            }
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pos(pub i8, pub i8);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PartialPos(pub Option<i8>, pub Option<i8>);

#[derive(Clone, Debug, PartialEq)]
pub enum ParseNotationErr {
    TooShort,
    InvalidColumn,
    InvalidRow,
}

impl Pos {
    pub fn unbounded(col: i8, row: i8) -> bool {
        !(0..8).contains(&col) || !(0..8).contains(&row)
    }

    /// Returns `None` when unbounded
    pub fn new_bounded(col: i8, row: i8) -> Option<Self> {
        if Self::unbounded(col, row) {
            return None;
        }
        Some(Self::new_unchecked(col, row))
    }

    /// Constructs a new `Pos` with bounded coordinates
    pub fn new_wrap(mut col: i8, mut row: i8) -> Self {
        if col >= 8 {
            col -= 8;
        }
        if row >= 8 {
            row -= 8;
        }
        while col < 0 {
            col += 8;
        }
        while row < 0 {
            row += 8;
        }
        Self::new_unchecked(col, row)
    }

    pub fn new_unchecked(col: i8, row: i8) -> Self {
        Self(col, row)
    }

    /// Constructs a position from chess notation
    pub fn from_notation(input: &str) -> Result<Self, ParseNotationErr> {
        if input.len() < 2 {
            return Err(ParseNotationErr::TooShort);
        }
        let mut chars = input.chars();
        let col = match chars.next().unwrap().to_ascii_lowercase() {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _ => return Err(ParseNotationErr::InvalidColumn),
        };
        let row = {
            let c = chars.next().unwrap();
            if !c.is_ascii_digit() {
                return Err(ParseNotationErr::InvalidRow);
            }
            c as i8 - 48
        };
        Ok(Self::new_wrap(col, row))
    }
}

impl PartialPos {
    /// Possible values are
    /// 1
    /// a
    /// a1
    pub fn from_coord(input: &str) -> Self {
        if input.is_empty() {
            return Self(None, None);
        }

        let mut chars = input.chars();
        let first = chars.next().unwrap();

        if first.is_ascii_digit() {
            return Self(None, Some(first as i8 - 48));
        }
        let col = match first.to_ascii_lowercase() {
            'a' => Some(0),
            'b' => Some(1),
            'c' => Some(2),
            'd' => Some(3),
            'e' => Some(4),
            'f' => Some(5),
            'g' => Some(6),
            'h' => Some(7),
            _ => None,
        };
        let row = match chars.next() {
            Some(x) => match x {
                x if x.is_ascii_digit() && (0..8).contains(&(x as i8 - 48)) => Some(x as i8 - 48),
                _ => None,
            },
            _ => None,
        };
        Self(col, row)
    }
}

impl From<(i8, i8)> for Pos {
    fn from(value: (i8, i8)) -> Self {
        Self::new_wrap(value.0, value.1)
    }
}

impl From<(u8, u8)> for Pos {
    fn from(value: (u8, u8)) -> Self {
        Self::new_unchecked(value.0 as i8, value.1 as i8)
    }
}

impl std::fmt::Display for ParseNotationErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parse Position Error: {}",
            match self {
                ParseNotationErr::TooShort => "text too short",
                ParseNotationErr::InvalidColumn => "invalid column character",
                ParseNotationErr::InvalidRow => "invalid row index",
            }
        )
    }
}
