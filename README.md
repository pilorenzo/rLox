# <center> rLox </center>
## <center> A Lox interpreter written in Rust </center>
</div>

---


This is a Rust port of the Lox interpreter.

The original version of the interpreter has been implemented by Robert Nystrom as described in his book, [Crafting Interpreters](https://craftinginterpreters.com/).

There are no particular differences to the original version of the interpreter, this project has been made for the sole purpose of learning Rust.

## Usage

---

You can interpret a file passing the path to the file after the executable 
(you can copy the file `test_file.rlox` in this repo).

```sh
$ /path/to/executable/rLox /path/to/script/file.rlox # extension does not matter
```

If you want to build the interpreter by source, you need to install [Rust](https://www.rust-lang.org/tools/install). Then, open a terminal, clone the project, go to the project directory and run:

```sh
cargo run ./file_test.rlox
```


If no file is passed, a REPL environment is opened.
