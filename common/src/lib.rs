#![warn(clippy::all, clippy::pedantic, clippy::nursery)]

pub mod two_dimensional_motion {

    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum Direction {
        North,
        South,
        East,
        West,
    }

    pub const ALL_DIRECTIONS: [Direction; 4] = [
        Direction::North,
        Direction::South,
        Direction::East,
        Direction::West,
    ];

    #[must_use]
    pub fn opposite(left: Direction, right: Direction) -> bool {
        #[allow(clippy::enum_glob_use)]
        use Direction::*;
        match left {
            North => right == South,
            South => right == North,
            East => right == West,
            West => right == East,
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Drc {
        pub d_row: i8,
        pub d_col: i8,
    }

    impl Drc {
        #[must_use]
        pub const fn d_row(&self) -> i8 {
            self.d_row
        }
        #[must_use]
        pub const fn d_col(&self) -> i8 {
            self.d_col
        }
    }

    #[must_use]
    pub const fn delta(dir: Direction) -> Drc {
        #[allow(clippy::enum_glob_use)]
        use Direction::*;
        match dir {
            North => Drc {
                d_row: -1,
                d_col: 0,
            },
            South => Drc { d_row: 1, d_col: 0 },
            East => Drc { d_row: 0, d_col: 1 },
            West => Drc {
                d_row: 0,
                d_col: -1,
            },
        }
    }
}

pub mod two_dimensional_map {
    use std::ops::{Index, IndexMut, Range};

    use crate::two_dimensional_motion::Direction;

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
    pub struct Location {
        row: usize,
        col: usize,
    }

    impl Location {
        #[must_use]
        pub const fn new(row: usize, col: usize) -> Self {
            Self { row, col }
        }

        #[must_use]
        #[allow(clippy::cast_sign_loss, clippy::cast_possible_wrap)]
        pub fn delta(&self, dir: Direction) -> Self {
            let delta = crate::two_dimensional_motion::delta(dir);
            Self {
                row: (self.row as isize + isize::from(delta.d_row)) as usize,
                col: (self.col as isize + isize::from(delta.d_col)) as usize,
            }
        }

        #[must_use]
        pub const fn row(&self) -> usize {
            self.row
        }

        #[must_use]
        pub const fn col(&self) -> usize {
            self.col
        }
    }

    pub struct Map<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> {
        locations: [[Object; COL_LENGTH]; ROW_LENGTH],
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object: Default + Copy> Default
        for Map<ROW_LENGTH, COL_LENGTH, Object>
    {
        fn default() -> Self {
            Self {
                locations: [[Object::default(); COL_LENGTH]; ROW_LENGTH],
            }
        }
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> Index<Location>
        for Map<ROW_LENGTH, COL_LENGTH, Object>
    {
        type Output = Object;

        fn index(&self, location: Location) -> &Self::Output {
            &self.locations[location.row][location.col]
        }
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> IndexMut<Location>
        for Map<ROW_LENGTH, COL_LENGTH, Object>
    {
        fn index_mut(&mut self, location: Location) -> &mut Self::Output {
            &mut self.locations[location.row][location.col]
        }
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> Index<(usize, usize)>
        for Map<ROW_LENGTH, COL_LENGTH, Object>
    {
        type Output = Object;

        fn index(&self, index: (usize, usize)) -> &Self::Output {
            &self.locations[index.0][index.1]
        }
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> IndexMut<(usize, usize)>
        for Map<ROW_LENGTH, COL_LENGTH, Object>
    {
        fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
            &mut self.locations[index.0][index.1]
        }
    }

    impl<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object> Map<ROW_LENGTH, COL_LENGTH, Object> {
        pub const fn in_range(location: Location, dir: Direction) -> bool {
            in_range(location, dir, ROW_LENGTH, COL_LENGTH)
        }

        pub const fn row_range(&self) -> Range<usize> {
            0..ROW_LENGTH
        }

        pub const fn col_range(&self) -> Range<usize> {
            0..COL_LENGTH
        }
    }

    pub const fn in_range(
        location: Location,
        dir: Direction,
        row_length: usize,
        col_length: usize,
    ) -> bool {
        #[allow(clippy::enum_glob_use)]
        use Direction::*;
        match dir {
            North => location.row > 0,
            South => location.row < row_length - 1,
            East => location.col < col_length - 1,
            West => location.col > 0,
        }
    }
}

pub mod two_dimensional_map_io {
    use std::io::BufRead;

    use crate::two_dimensional_map::{Location, Map};

    pub fn read_input<const ROW_LENGTH: usize, const COL_LENGTH: usize, Object: Default + Copy>(
        filename: String,
        deserialize: &dyn Fn(char) -> Object,
    ) -> Map<ROW_LENGTH, COL_LENGTH, Object> {
        let mut map = Map::default();
        let file = std::fs::File::open(filename).expect("where's my input?!?");
        let lines = std::io::BufReader::new(file).lines();
        for (row, line) in lines.enumerate() {
            let line = line.expect("we read a line without reading it?!?");
            for (col, symbol) in line.chars().enumerate() {
                map[Location::new(row, col)] = deserialize(symbol);
            }
        }
        map
    }
}

#[cfg(test)]
mod tests {
    use crate::{two_dimensional_map::Location, two_dimensional_motion::Direction};

    #[test]
    fn location_deltas() {
        let origin = Location::new(10, 20);
        let above = origin.delta(Direction::North);
        let below = origin.delta(Direction::South);
        let left = origin.delta(Direction::West);
        let right = origin.delta(Direction::East);

        assert_eq!(above.row(), origin.row() - 1);
        assert_eq!(above.col(), origin.col());
        assert_eq!(below.row(), origin.row() + 1);
        assert_eq!(below.col(), origin.col());

        assert_eq!(left.col(), origin.col() - 1);
        assert_eq!(left.row(), origin.row());
        assert_eq!(right.col(), origin.col() + 1);
        assert_eq!(right.row(), origin.row());
    }
}
