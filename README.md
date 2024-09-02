<div class="Image" align="center">
  <img src="https://github.com/aryaman-sakthi/Scrabble-Move-Generator/blob/main/assets/Project%20Thumbnail.jpeg" alt="Logo" width="150" height="150">
</div>
<h1 align="center">Scrabble Move Generator and Trie Library</h1>

## About:
This project involves the development of two interrelated components:  

* **Trie Library:** A custom Haskell implementation of a Trie data structure, optimized for dictionary operations such as lookup and insert.
* **Scrabble Move Generator:** A move generator for Scrabble-like word games, leveraging the Trie library to efficiently determine legal word placements on a game board.

## Built With:
![Haskell](https://img.shields.io/badge/Haskell-5D4F85?logo=haskell&logoColor=fff&style=for-the-badge)

## Project Structure
### Trie Library
The Trie is a tree-like data structure where each node represents a letter or an end-of-word marker. It's particularly useful for storing and searching dictionaries.

#### Features
* **fromList:** Converts a list of words into a Trie.
* **wellFormed:** Ensures that the Trie is correctly structured.
* **minimal:** Optimizes the Trie by removing unnecessary nodes.
* **prune:** Prunes the Trie to remove non-essential branches.
* **check:** Validates if a given word exists in the Trie.
* **union:** Combines two Tries into one.
* **intersection:** Finds common words between two Tries.
* **Monoid instance:** Implements Haskell’s Monoid interface for Tries, allowing for flexible combination operations.

### Scrabble Move Generator
The move generator creates legal Scrabble moves given a set of constraints and a current board state. It makes use of the Trie library to efficiently validate word formations.

#### Features
* **pick:** Selects tiles from the rack that match a given pattern.
* **sandwichableLetters:** Determines which letters can legally connect words vertically and horizontally.
* **filterLength:** Filters potential moves by the length of the word.
* **filterPattern:** Matches a pattern of constraints against the Trie to find valid words.
* **filterPlayables:** Determines playable words based on the player's available tiles.
* **moves:** Generates all possible legal moves for the current board state.
* **allMoves:** Generates all legal moves for every possible starting position on the board.

## Data Structures:
* **Trie:** A Trie is used to efficiently store the dictionary.
* **Constraint:** Represents a condition a character must meet, such as matching a specific character or being a wildcard.
* **Pattern:** A sequence of constraints representing a word pattern.
* **Tile:** Represents a letter tile or a blank tile (wildcard).
* **Rack:** A collection of tiles available to the player.
* **Board:** Represents the game board, including existing tiles and empty spaces.

## Conclusion:
This project highlights the use of Haskell's functional programming strengths to create an efficient Trie library and Scrabble move generator. By utilizing the Trie data structure, the move generator efficiently handles large dictionaries and generates legal Scrabble moves based on current board states and player racks. This implementation demonstrates how Haskell can manage complex game logic with clarity and precision, serving as a valuable tool for building intelligent word game applications.
