# Minesweeper in Haskell

This is a fully functional, terminal-based implementation of the classic Minesweeper game written in Haskell. The project follows principles of functional programming, emphasizing modularity, testability, and performance. The game supports user interaction through a CLI, random mine placement, proper win/loss handling, and robust testing.

## Features

- Console-based gameplay
- Randomized mine placement with first-move safety
- Recursive and cascading cell reveal logic
- Flagging and unflagging of cells
- Win/loss detection and error handling
- Modular architecture using Haskell’s powerful type system
- Unit tests and property-based tests using Tasty and Hedgehog

## Project Structure

- `Cell.hs` – Defines the `Cell` type (mine, revealed, flagged, unrevealed).
- `Grid.hs` – Initializes and operates on the game grid using `Data.Vector`.
- `Game.hs` – Contains game logic: move processing, win/loss conditions, cascading reveals.
- `Main.hs` – Handles user input and game loop.

## Technical Design

### Grid Representation
Originally implemented with lists, the grid was optimized using `Data.Vector` to ensure efficient access (O(1)) and modifications.

### Cascading Reveal
A recursive approach was used to simulate a depth-first traversal for revealing neighboring cells with zero adjacent mines. This could later be optimized into an iterative BFS-style approach to avoid stack overflow on large grids.

### Error Handling
Used `Either String` for handling and propagating game errors clearly and functionally.

### Testing
- **Hedgehog** for property-based testing (random input generation).
- **Tasty** to run both unit and property tests together.

## Example Tests

- Valid move scenarios
- Edge cases (first-move exclusion, invalid coordinates)
- Property: no mines adjacent to the first move
- Recursive reveal correctness

## Known Limitations

- Recursive reveal logic may cause stack overflow on very large grids.
- Future optimization could involve replacing recursion with iteration.

## Dependencies

- `vector`
- `tasty`
- `hedgehog`
- `random`

## Getting Started

Make sure you have [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) installed.

```bash
stack build
stack run
stack test