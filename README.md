## Minesweeper, but in Elm

Just messing around and learning Elm by building minesweeper. Nothing fancy.

Play it here: https://minesweeper-elm.calvinkcollins.com/

## Stuff I learned building this

Writing this down mostly so future me doesn't have to relearn it.

### Elm's type safety is awesome but it makes you check dumb stuff

The compiler makes you handle every case of every custom type and every `Maybe`. Most of the time that's great, it genuinely catches real bugs (the [Elm guide's whole pitch on `Maybe` vs `null`](https://guide.elm-lang.org/error_handling/maybe.html) is basically "no more null pointer exceptions"). But it also means you end up writing `Nothing` branches and catch-alls for stuff that logically can never happen given how you actually call the code. It just doesn't know that.

Some examples I ran into:

- [`Utils.getCell`](src/Utils.elm) returns a `Maybe CellType` because it's indexing into a list by row/col and lists don't know if your index is in bounds. Fine. Except [`revealCell`](src/Utils.elm) only ever calls it with offsets around a cell that's already confirmed to exist, so like 90% of the `Nothing` handling there is dead code I was forced to write anyway.
- [`Cell.updateCellComponent`](src/Cell.elm) has a `Nothing -> model` branch for `model.minefield` on line 26, but a cell message literally can't fire unless a minefield is already on the screen. Compiler doesn't care, still makes me write it.
- The color lookup for `surroundingMines` in [`Cell.renderCellComponent`](src/Cell.elm:113) needs a `_ -> "black"` at the end even though that number is realistically only ever 0-8.

### Turns out the fix is "make impossible states impossible", which I only sort of did

There's this whole idea in the Elm world (it's literally on the [custom types page](https://guide.elm-lang.org/types/custom_types.html) of the guide) where instead of trusting yourself to keep a bunch of fields in sync, you design your model so the bad combination just can't be represented. I did not fully do that here. [`Types.Model`](src/Types.elm) has `gameStatus`, `minefield`, `gameConfig`, and `difficulty` as four separate fields, so nothing in the type system actually stops you from having `gameStatus == Started` and `minefield == Nothing` at the same time. It just... doesn't happen, because I was careful. That's the whole problem though, "I was careful" isn't a guarantee, it's a hope.

The better version would probably fold minefield + config into the `GameStatus` variants themselves, something like `Started { config : GameConfig, minefield : Minefield }`. Then "playing implies there's a minefield" is just true, and all those extra `Maybe` checks evaporate.

### Random other things that stuck with me

- No mutation means flood-filling the empty cells (the classic reveal-a-bunch-of-zeros minesweeper thing) turns into two functions calling each other recursively ([`revealCell`](src/Utils.elm) and [`revealSurroundingCells`](src/Utils.elm)). The "have I already visited this" state has to get threaded through return values instead of just being a mutable set somewhere.
- `Random.Generator` composes really nicely. [`placeMines`](src/Utils.elm) uses `Random.andThen` to chain N mine placements together, and `placeMine` uses `Random.map2` to pick a row and a column at the same time. Once it clicked it felt very natural.
- Core `List` has no get-by-index. At all. So anything grid-like ends up pulling in `elm-community/list-extra` for `List.Extra.getAt`.

## Things I still want to build

Ideas for later, mostly picked because they'd force me to touch parts of Elm I haven't used yet.

### Fix the model

- Actually do the model refactor from above. Collapse `gameStatus` / `gameConfig` / `minefield` / `difficulty` into one type, something like `Select | Playing {...} | Won {...} | Lost {...}`, and see how much of the `Maybe`-unwrapping nonsense just disappears on its own.

### Actual features

- **First click can't be a mine.** Real minesweeper does this. Right now [`generateMinefield`](src/Utils.elm) drops mines before you've clicked anything at all, so I'd need to either reroll the board or generate mines after the first click instead.
- **A timer.** Need a subscription for this, [`subscriptions`](src/Main.elm) is just `Sub.none` right now. Probably `Time.every` and a `Tick` message.
- **Mine counter** (mines minus flags placed). Easy one, just a fold over the board.
- **Pick your own difficulty** instead of the three presets. Needs a text input, `String.toInt`, and deciding what happens with bad input — a good excuse to finally use `Result`, which I haven't touched anywhere in this project.
- **Keyboard controls**, arrow keys + space to reveal + f to flag. Would need `Browser.Events.onKeyDown` and a "currently selected cell" concept that doesn't exist yet.
- **Remember your best time / last difficulty** across page reloads. Elm can't touch localStorage by itself so this means writing my first ever port, plus actually using `elm/json` for encoding/decoding (it's a dependency already and I don't use it for anything).
- **Shareable boards via a seed in the URL.** `Url.Parser` is imported in [`Main.elm`](src/Main.elm) already but only for navigation, never actually parsed for anything. Could pair it with `Random.initialSeed`.

### Tooling I never bothered with

- **elm-test.** Zero tests in this whole repo (`test-dependencies` in `elm.json` is empty, lol). [`checkGameStatus`](src/Utils.elm), [`countSurroundingMines`](src/Utils.elm), and the flood fill are all pure functions just sitting there waiting to be tested.
- **elm-review.** Never set it up. Codebase is probably big enough now to be worth it.
