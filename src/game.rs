use thiserror::Error;

pub mod moves;
pub mod pieces;

pub(crate) use moves::*;
pub(crate) use pieces::*;

#[derive(Clone, Debug, PartialEq)]
/// Board struct holding the context of a chess game.
///
/// The grid is structured as follows:
/// [col, col, ..., col]
/// each column would have [piece, piece, ..., piece]
/// so the grid would be [[piece, ...], [piece, ...], ...]
/// to access a piece at row and col, use grid[row * 8 + col]
pub struct Board {
    pub grid: [Option<Piece>; 64],
    pub turn: PieceColor,
    pub winner: Option<PieceColor>,
    pub in_check: Option<PieceColor>,
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
            in_check: None,
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

    pub fn apply_move(&mut self, pmove: Move) -> Result<(), ApplyMoveErr> {
        // No need to continue if we have a winner already
        if self.winner.is_some() {
            return Ok(());
        }
        // Make sure only the current turn can do a move
        if self.turn != pmove.piece_color {
            return Err(ApplyMoveErr::TurnErr(self.turn));
        }

        // Check for checks
        if let Some(c) = self.in_check {
            let mut clone = self.clone();
            clone.in_check = None; // Ignore check checks for clone
            let _ = clone.apply_move(pmove);
            if clone.get_checks(pmove.piece_color).is_some() {
                return Err(ApplyMoveErr::InCheck(c));
            }
        }

        // Move the piece
        let piece = self.grid[Self::pos_to_idx(pmove.from)];
        self.grid[Self::pos_to_idx(pmove.from)] = None;
        self.grid[Self::pos_to_idx(pmove.to)] = piece;

        // Update whose turn is next
        self.turn = self.turn.invert();

        // Update winner if king is captured
        if let Some(p) = pmove.captures
            && p == PieceType::King
        {
            self.winner = Some(pmove.piece_color);
        }

        // Update board check sate
        if self.get_checks(pmove.piece_color.invert()).is_some() {
            self.in_check = Some(pmove.piece_color.invert());
        } else {
            self.in_check = None;
        }

        Ok(())
    }

    pub fn get_checks(&self, turn: PieceColor) -> CheckType {
        // Find king piece of turn's respective color
        for (i, square) in self.grid.iter().enumerate() {
            match square {
                None => continue,
                Some(x) => {
                    if x.piece_type != PieceType::King || x.piece_color != turn {
                        continue;
                    }

                    // Get coordinate for ease of checking
                    dbg!(i / 8, i % 8);
                    let pos = Pos::new_unchecked(i as i8 / 8, i as i8 % 8);

                    // Store checks found
                    // Maximum of 2 because higher is impossible to achieve anyways
                    let mut found = [None, None];

                    // Define macro for ease of checking
                    // Return true to exit get_checks if found checking piece
                    // Else return whether a piece exists or not for line of sight blocking
                    macro_rules! store_check {
                        ($pos: expr) => {
                            if found[0].is_none() {
                                found[0] = Some($pos);
                            } else {
                                found[1] = Some($pos);
                                return CheckType::Double(found[0].unwrap(), found[1].unwrap());
                            }
                        };
                    }
                    macro_rules! check {
                        ($x: expr, $y: expr, $group: expr) => {
                            if let Some(f) = Pos::new_bounded($x, $y) {
                                if let Some(p) = self.get_piece(f)
                                    && PieceTypeGroup::has($group, p.piece_type)
                                    && p.piece_color != turn
                                {
                                    store_check!(f)
                                }
                                self.get_piece(f).is_none()
                            } else {
                                false
                            }
                        };
                    }

                    // The amount of repitition here actually disgusts me
                    // Check cardinals
                    use PieceType::*;
                    for step in 0..8 {
                        if check!(pos.0 - step, pos.1, Rook.to_group() | Queen.to_group()) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(pos.0 - step, pos.1, Rook.to_group() | Queen.to_group()) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(pos.0 + step, pos.1, Rook.to_group() | Queen.to_group()) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(pos.0, pos.1 - step, Rook.to_group() | Queen.to_group()) {
                            break;
                        }
                    }
                    // Check diagonals
                    for step in 0..8 {
                        if check!(
                            pos.0 - step,
                            pos.1 - step,
                            Bishop.to_group() | Queen.to_group()
                        ) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(
                            pos.0 + step,
                            pos.1 - step,
                            Bishop.to_group() | Queen.to_group()
                        ) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(
                            pos.0 - step,
                            pos.1 + step,
                            Bishop.to_group() | Queen.to_group()
                        ) {
                            break;
                        }
                    }
                    for step in 0..8 {
                        if check!(
                            pos.0 + step,
                            pos.1 + step,
                            Bishop.to_group() | Queen.to_group()
                        ) {
                            break;
                        }
                    }
                    // Knights
                    check!(pos.0 - 2, pos.1 - 1, Knight.to_group());
                    check!(pos.0 - 2, pos.1 + 1, Knight.to_group());
                    check!(pos.0 - 1, pos.1 - 2, Knight.to_group());
                    check!(pos.0 - 1, pos.1 + 2, Knight.to_group());
                    check!(pos.0 + 2, pos.1 - 1, Knight.to_group());
                    check!(pos.0 + 2, pos.1 + 1, Knight.to_group());
                    check!(pos.0 + 1, pos.1 - 2, Knight.to_group());
                    check!(pos.0 + 1, pos.1 + 2, Knight.to_group());
                    // Pawns
                    check!(pos.0 + 1, pos.1 + turn.sign(), Pawn.to_group());
                    check!(pos.0 - 1, pos.1 + turn.sign(), Pawn.to_group());
                }
            }
        }
        CheckType::None
    }
}
