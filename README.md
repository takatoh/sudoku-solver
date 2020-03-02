sudoku-solver
=============

数独とかナンプレとか呼ばれるパズルを解くプログラムです。

コンパイルするには Haskell のコンパイラが必要です。

Install
-------

GitHub からクローンして

    git clone https://github.com/takatoh/sudoku-solver.git

make します。

    make build

sudoku.exe ファイルができるので、パスの通った適当なディレクトリにコピーしてください。

Usage
-----

問題の入力ファイルを作ります。入力ファイルはテキストファイルで、9×9マスの問題を
9文字×9行に書きます。このとき、数字の入っているマスはその数字を、空白のマスは
空白文字とします。

できた入力ファイル（例えば example.txt）を引数にして sudoku.exeを
実行します。

    sudoku.exe example.txt

うまく解ければ、答えが表示されます。

License
-------

MIT License
