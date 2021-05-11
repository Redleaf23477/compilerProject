# HW1 Notes

## Supporting

Note: golden scanner doesn't deal with edge cases such as trailing zeros or leading zeros

- `[sigma]` = those listed in spec p.35
- `[special-char]` = `[\'"]`
- `[normal-char]` = `[sigma]` - `[special-char]`
- `[esc-seq]` = `\[\'"0tn]`
- `[c-char]` = `[normal-char]+[esc-seq]`

| Done? | What                 | Regular Expression                   |
| ----- | -------------------- | ------------------------------------ |
| x     | key words            | (predefined set)                     |
| x     | macros               | (predefined set)                     |
| x     | identifiers          | `[a-zA-z_][a-zA-Z0-9_]*`             |
| x     | operators            | (predefined set)                     |
| x     | punctuations         | (predefined set)                     |
| x     | integer constant     | `[\+-]?[0-9]+`                       |
| x     | float point constant | `[\+-]?[0-9]+\.[0-9]*`               |
| x     | string constant      | `"[c-char]*"`                        |
| x     | char constant        | `'[c-char]'`                         |
| x     | comments (line)      | `//[sigma]*`                         |
| x     | comments (block)     | `/*` start state; `*/` end state     |
| x     | pragma               | (predefined entire line, include \n) |

## Precedence

- [x] 先來先贏
    - [x] line comment
    - [x] block comment
    - [x] string constant
    - [x] char constant
- [x] pragma
- [x] float point constant
- [x] integer constant
- [x] operators
- [x] punctuations 
- [x] key words
- [x] macros
- [x] identifiers
